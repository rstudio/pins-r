#' Use an Azure storage container as a board
#'
#' Pin data to a container in Azure storage using the AzureStor package.
#'
#' @inheritParams new_board
#' @param container An azure storage container created by
#'   [AzureStor::blob_container()] or similar.
#' @param path Path to the directory in the container to store pins. Will be
#'   created if it doesn't already exist. The equivalent of a `prefix` for AWS
#'   S3 storage.
#' @param n_processes Maximum number of processes used for parallel
#'   uploads/downloads.
#' @details
#' You can create a board in any of the services that AzureStor supports: blob
#' storage, file storage and Azure Data Lake Storage Gen2 (ADLSgen2).
#'
#' Blob storage is the classic storage service that is most familiar to people,
#' but is relatively old and inefficient. ADLSgen2 is a modern replacement API
#' for working with blobs that is much faster when working with directories.
#' You should consider using this rather than the classic blob API where
#' possible; see the examples below.
#'
#' `board_azure()` is powered by the AzureStor package, which is a suggested
#' dependency of pins (not required for pins in general). If you run into errors
#' when deploying content to a server like <https://www.shinyapps.io> or
#' [Connect](https://posit.co/products/enterprise/connect/), add
#' `requireNamespace("AzureStor")` to your app or document for [automatic dependency
#' discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).
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
#' # First, we create a board using the classic Azure blob API
#' url <- "https://myaccount.blob.core.windows.net/mycontainer"
#' container <- AzureStor::blob_container(url, sas = "my-sas")
#' board <- board_azure(container, "path/to/board")
#' board %>% pin_write(iris)
#'
#' # ADLSgen2 is a modern, efficient way to access blobs
#' # - Use 'dfs' instead of 'blob' in the account URL to use the ADLSgen2 API
#' # - Use the 'storage_container' generic instead of the service-specific
#' #   'blob_container'
#' # - We reuse the board created via the blob API above
#' adls_url <- "https://myaccount.dfs.core.windows.net/mycontainer"
#' container <- AzureStor::storage_container(adls_url, sas = "my-sas")
#' board <- board_azure(container, "path/to/board")
#' board %>% pin_list()
#' board %>% pin_read("iris")
#' }
board_azure <- function(container, path = "", n_processes = 10, versioned = TRUE, cache = NULL) {
  check_installed("AzureStor")

  if (path == "/") {
    path <- ""
  }
  board_path <- fs::path(container$endpoint$url, container$name, path)
  cache <- cache %||% board_cache_path(paste0("azure-", hash(board_path)))
  if (path != "") {
    if (inherits(container, "file_share")) {
      try(AzureStor::create_storage_dir(container, path, recursive = TRUE), silent = TRUE)
    } else {
      AzureStor::create_storage_dir(container, path)
    }
  }

  new_board_v1(
    "pins_board_azure",
    name = "azure",
    container = container,
    path = path,
    n_processes = n_processes,
    cache = cache,
    versioned = versioned
  )
}

board_azure_test <- function(path, type = c("blob", "file", "dfs"), ...) {
  skip_if_missing_envvars("board_azure()", "PINS_AZURE_KEY")

  type <- arg_match(type)
  acct_name <- Sys.getenv("PINS_AZURE_ACCOUNT")
  acct_url <- sprintf("https://%s.%s.core.windows.net/pins-rstats-testing-ci",
                      acct_name, type)

  container <- AzureStor::storage_container(
    acct_url,
    key = Sys.getenv("PINS_AZURE_KEY")
  )
  board_azure(container, path = path, cache = tempfile(), n_processes = 2, ...)
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

  metadata_absolute_path <- azure_normalize_path(board, metadata_blob)
  if (!AzureStor::storage_file_exists(board$container, metadata_absolute_path)) {
    abort_pin_version_missing(version)
  }
  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  azure_download(board, metadata_blob, progress = FALSE)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    name = name,
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
  check_pin_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  version_dir <- azure_normalize_path(board, name, version)

  # Upload metadata
  local_azure_progress(FALSE)
  azure_upload_file(
    board,
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

#' @export
write_board_manifest_yaml.pins_board_azure <- function(board, manifest, ...) {

  paths <- AzureStor::list_storage_files(board$container, info = "name")

  if (manifest_pin_yaml_filename %in% paths) {
    AzureStor::delete_storage_file(
      board$container,
      manifest_pin_yaml_filename,
      confirm = FALSE
    )
  }

  temp_file <- withr::local_tempfile()
  yaml::write_yaml(manifest, file = temp_file)
  azure_upload_file(board, src = temp_file, dest = manifest_pin_yaml_filename)
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_azure <- function(x, ...) {
  ellipsis::check_dots_empty()
  "AzureStor"
}

# Helpers -----------------------------------------------------------------

azure_delete_dir <- function(board, dir) {
  dir <- azure_normalize_path(board, dir)
  AzureStor::delete_storage_dir(
    board$container,
    dir,
    confirm = FALSE,
    recursive = TRUE
  )
}

azure_ls <- function(board, dir = "") {
  dir <- azure_normalize_path(board, dir)

  paths <- AzureStor::list_storage_files(
    board$container,
    dir = dir,
    recursive = FALSE
  )
  unique(fs::path_file(paths$name[paths$isdir] %||% character(0)))
}

azure_dir_exists <- function(board, path) {
  dir <- azure_normalize_path(board, path)
  AzureStor::storage_dir_exists(board$container, dir)
}

local_azure_progress <- function(progress = is_interactive(), env = parent.frame()) {
  withr::local_options(list(azure_storage_progress_bar = progress), .local_envir = env)
}

azure_download <- function(board, keys, progress = is_interactive()) {
  local_azure_progress(progress)

  paths <- fs::path(board$cache, keys)
  keys <- azure_normalize_path(board, keys)
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
  if (inherits(board$container, "file_share")) {
    AzureStor::storage_upload(board$container, src, dest, create_dir = TRUE)
  } else {
    AzureStor::storage_upload(board$container, src, dest)
  }
}

azure_normalize_path <- function(board, ...) {
  path <- fs::path(board$path, ...)
  # blob storage doesn't like '/foo' paths with leading '/' and non-empty 'foo'

  bads <- nchar(path) > 1 & grepl("^/", path)
  path[bads] <- substr(path[bads], 2, nchar(path[bads]))
  path
}
