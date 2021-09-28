#' Use an Azure Blob Storage Container as a board
#'
#' Pin data to a container on Azure's blog storage using the AzureStor package.
#'
#' @inheritParams new_board
#' @param container An azure storage container created by
#'   [AzureStor::blob_container()] or similar.
#' @param n_processes Maximum number of processes used for parallel
#'   uploads/downloads.
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
#' board <- board_azure(container)
#' board %>% pin_write(iris)
#' }
board_azure <- function(container, n_processes = 10, versioned = TRUE, cache = NULL) {
  check_installed("AzureStor")

  cache <- cache %||% board_cache_path(paste0("azure-", hash(container$endpoint$url)))
  new_board_v1("pins_board_azure",
    name = "azure",
    container = container,
    n_processes = n_processes,
    cache = cache,
    versioned = versioned
  )
}

board_azure_test <- function(...) {
  skip_if_missing_envvars("board_azure()", "PINS_AZURE_SAS")

  container <- AzureStor::blob_container(
    "https://pins.blob.core.windows.net/test-data",
    sas = Sys.getenv("PINS_AZURE_SAS")
  )
  board_azure(container, cache = tempfile(), n_processes = 2, ...)
}

#' @export
pin_list.pins_board_azure <- function(board, ...) {
  azure_ls(board)
}

#' @export
pin_exists.pins_board_azure <- function(board, name, ...) {
  length(azure_ls(board, name)) > 0
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

  if (!AzureStor::storage_file_exists(board$container, metadata_blob)) {
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

  version_dir <- fs::path(name, version)

  # Upload metadata
  local_azure_progress(FALSE)
  AzureStor::storage_upload(board$container,
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
  expr(board_azure(!!container))
}


# Helpers -----------------------------------------------------------------

azure_delete_dir <- function(board, dir) {
  ls <- AzureStor::list_storage_files(board$container, dir)
  for (path in ls$name) {
    AzureStor::delete_storage_file(board$container, path, confirm = FALSE)
  }
}

azure_ls <- function(board, dir = "/") {
  ls <- AzureStor::list_storage_files(
    board$container,
    dir = dir,
    recursive = FALSE
  )
  paths <- ls$name

  if (dir != "/") {
    # trim off name/ prefix
    paths <- substr(paths, nchar(dir) + 2, nchar(paths))
  }
  # trim / suffix off directories
  paths <- sub("/$", "", paths)

  paths
}



local_azure_progress <- function(progress = !is_testing(), env = parent.frame()) {
  withr::local_options(azure_storage_progress_bar = progress, .local_envir = env)
}

azure_download <- function(board, keys, progress = !is_testing()) {
  local_azure_progress(progress)

  paths <- fs::path(board$cache, keys)
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
