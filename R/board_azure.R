#' Use an Azure storage container as a board
#'
#' Pin data to a container in Azure storage using the AzureStor package.
#'
#' @inheritParams new_board
#' @param container An azure storage container created by
#'   [AzureStor::blob_container()] or similar.
#' @param path Path to directory to store pins. Will be created if it
#'   doesn't already exist.
#' @param n_processes Maximum number of processes used for parallel
#'   uploads/downloads.
#' @param hierarchical_namespace_enabled (Blob storage only.) Whether the storage account has the
#'   [hierarchical namespace](https://docs.microsoft.com/en-us/azure/storage/blobs/data-lake-storage-namespace)
#'   feature, which enables fast and efficient processing of data that is
#'   organised into directories.
#' @export
#' @examples
#' if (requireNamespace("AzureStor")) {
#'   # Public access board
#'   url <- "https://pins.blob.core.windows.net/public-data"
#'   container <- AzureStor::blob_container(url)
#'   board <- board_azure(container)
#'   board %>% pin_read("mtcars")
#' }
#'
#' \dontrun{
#' # To create a board that you can write to, you'll need to supply one
#' # of `key`, `token`, or `sas` to AzureStor::blob_container()
#' container <- AzureStor::blob_container(url, key = "my-key")
#' board <- board_azure(container, "path/to/board")
#' board %>% pin_write(iris)
#' }
board_azure <- function(container, path = "/", n_processes = 10, versioned = TRUE, cache = NULL) {
  check_installed("AzureStor")

  cache <- cache %||% board_cache_path(paste0("azure-", hash(container$endpoint$url)))
  if (path != "/") {
    if(inherits(container, "file_share")) {
      try(AzureStor::create_storage_dir(container, path, recursive = TRUE), silent = TRUE)
    } else {
      AzureStor::create_storage_dir(container, path)
    }
  }

  new_board_v1("pins_board_azure",
    name = "azure",
    container = container,
    path = path,
    n_processes = n_processes,
    cache = cache,
    versioned = versioned
  )
}

board_azure_test <- function(type = c("blob", "file", "dfs"), ...) {
  skip_if_missing_envvars("board_azure()", "PINS_AZURE_SAS")

  type <- arg_match(type)
  acct_name <- Sys.getenv("PINS_AZURE_ACCOUNT", "pins")
  acct_url <- sprintf("https://%s.%s.core.windows.net/test-data", acct_name, type)

  container <- AzureStor::storage_container(
    acct_url,
    sas = Sys.getenv("PINS_AZURE_SAS")
  )
  board_azure(container, path = "test/path", cache = tempfile(), n_processes = 2, ...)
}

#' @export
pin_list.pins_board_azure <- function(board, ...) {
  azure_ls(board)
}

#' @export
pin_exists.pins_board_azure <- function(board, name, ...) {
  azure_dir_exists(board, name)
}

#' @export
pin_delete.pins_board_azure <- function(board, names, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    azure_delete_dir(board, name)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_azure <- function(board, name, ...) {
  check_pin_exists(board, name)
  version_from_path(azure_ls(board, name))
}

#' @export
pin_version_delete.pins_board_azure <- function(board, name, version, ...) {
  azure_delete_dir(board, fs::path(name, version))
}

#' @export
pin_meta.pins_board_azure <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_blob <- fs::path(name, version, "data.txt")

  metadata_absolute_path <- fs::path(board$path, metadata_blob)
  if (!AzureStor::storage_file_exists(board$container, metadata_absolute_path)) {
    abort_pin_version_missing(version)
  }
  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  azure_download(board, metadata_blob, progress = FALSE)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_azure <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  keys <- map_chr(meta$file, ~ fs::path(name, meta$local$version, .x))
  azure_download(board, keys)

  meta
}

#' @export
pin_store.pins_board_azure <- function(board, name, paths, metadata,
                                       versioned = NULL, ...) {
  check_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  version_dir <- fs::path(board$path, name, version)

  # Upload metadata
  local_azure_progress(FALSE)
  azure_upload_file(board,
    src = textConnection(yaml::as.yaml(metadata)),
    dest = fs::path(version_dir, "data.txt")
  )

  # Upload files
  local_azure_progress()
  keys <- fs::path(version_dir, fs::path_file(paths))
  AzureStor::storage_multiupload(
    board$container,
    src = paths,
    dest = keys,
    max_concurrent_transfers = board$n_processes
  )

  name
}


#' @rdname board_deparse
#' @export
board_deparse.pins_board_azure <- function(board, ...) {
  container <- check_board_deparse(board, "container")
  url <- paste0(container$endpoint$url, container$name)

  container <- expr(AzureStor::storage_container(!!url))
  expr(board_azure(!!container, path = !!board$path))
}


# Helpers -----------------------------------------------------------------

azure_delete_dir <- function(board, dir) {
  dir <- fs::path(board$path, dir)

  # need 3 different ways of deleting a dir
  # - blob storage: delete all files in dir and subdirs
  # - file storage: delete files/dirs in reverse order, using correct function for each
  # - adlsgen2: deleting dir will delete contents automatically
  if (inherits(board$container, "blob_container")) {
    ls <- AzureStor::list_storage_files(board$container, dir, recursive = TRUE)
    for (path in ls$name) {
      AzureStor::delete_storage_file(board$container, path, confirm = FALSE)
    }

  } else if (inherits(board$container, "file_share")) {
    ls <- AzureStor::list_storage_files(board$container, dir, recursive = TRUE)
    for (i in rev(seq_len(nrow(ls)))) {
      if(ls$isdir[i]) {
        AzureStor::delete_storage_dir(board$container, ls$name[i], confirm = FALSE)
      } else {
        AzureStor::delete_storage_file(board$container, ls$name[i], confirm = FALSE)
      }
    }

  } else if (inherits(board$container, "adls_filesystem")) {
    AzureStor::delete_storage_dir(board$container, dir, confirm = FALSE, recursive = TRUE)

  } else {
    abort("Unknown Azure storage container type")
  }
}

azure_ls <- function(board, dir = "/") {
  dir <- fs::path(board$path, dir)

  paths <- AzureStor::list_storage_files(
    board$container,
    dir = dir,
    recursive = FALSE,
    info = "name"
  )
  fs::path_file(paths)
}


azure_dir_exists <- function(board, path) {

  # need 3 different ways of testing if a dir exists (!)
  # - blob storage: test if file list is non-empty
  # - file storage: test if trying to get file list throws error
  # - adlsgen2: use storage_file_exists()
  if (inherits(board$container, "blob_container")) {
    length(azure_ls(board, path)) > 0

  } else if (inherits(board$container, "file_share")) {
    !inherits(try(azure_ls(board, path), silent = TRUE), "try-error")

  } else if (inherits(board$container, "adls_filesystem")) {
    path <- fs::path(board$path, path)
    AzureStor::storage_file_exists(board$container, path)

  } else {
    abort("Unknown Azure storage container type")
  }
}

local_azure_progress <- function(progress = !is_testing(), env = parent.frame()) {
  withr::local_options(azure_storage_progress_bar = progress, .local_envir = env)
}

azure_download <- function(board, keys, progress = !is_testing()) {
  local_azure_progress(progress)

  paths <- fs::path(board$cache, keys)
  keys <- fs::path(board$path, keys)
  needed <- !fs::file_exists(paths)
  if (any(needed)) {
    AzureStor::storage_multidownload(
      board$container, keys[needed], paths[needed],
      max_concurrent_transfers = board$n_processes
    )
    fs::file_chmod(paths[needed], "u=r")
  }

  invisible()
}

azure_upload_file <- function(board, src, dest) {
  if(inherits(board$container, "file_share")) {
    AzureStor::storage_upload(board$container, src, dest, create_dir = TRUE)
  } else {
    AzureStor::storage_upload(board$container, src, dest)
  }
}
