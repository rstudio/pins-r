#' Use an Azure Blob Storage Container as a board
#'
#' Pin data to a container on Azure's blog storage using the AzureStor package.
#'
#' @inheritParams new_board
#' @param account,container Account and container name.
#' @param key,token,sas Authentication credentials: either an access `key`, an
#'   Azure Active Directory (AAD) `token`, or a `SAS`, in that order of
#'   priority. If no omitted, provides read access to a public (anonymous) board.
#' @param url If omitted, `board_azure()` will generate a endpoint url of the
#'   form `"https://{account}.blob.core.windows.net/{container}"`. Use this argument
#'   to override if needed.
#' @export
#' @examples
#' if (requireNamespace("AzureStor")) {
#'   # Public access board
#'   board <- board_azure("pins", "public-data")
#'   board %>% pin_read("mtcars")
#' }
#'
#' \dontrun{
#' # To create a board that you can write to, you'll need to supply one
#' # of `key`, `token`, or `sas`
#' board <- board_azure("pins", "private-data", sas = "...")
#' board %>% pin_write(iris)
#' }
board_azure <- function(
                        account,
                        container,
                        versioned = TRUE,
                        key = NULL,
                        token = NULL,
                        sas = NULL,
                        url = NULL,
                        cache = NULL) {

  check_installed("AzureStor")

  url <- url %||% paste0("https://", account, ".blob.core.windows.net/", container)

  # TODO: check error message when container doesn't exist
  container <- AzureStor::blob_container(url, key = key, token = token, sas = sas)

  cache <- cache %||% board_cache_path(paste0("azure-", hash(url)))
  new_board_v1("pins_board_azure",
    name = "azure",
    container = container,
    cache = cache,
    versioned = versioned
  )
}

board_azure_test <- function(...) {
  if (!has_envvars("PINS_AZURE_TEST_SAS")) {
    testthat::skip("PINS_AZURE_TEST_SAS not set")
  }

  board_azure("pins", "test-data",
    sas = Sys.getenv("PINS_AZURE_TEST_SAS"),
    cache = tempfile(), ...
  )
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

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  azure_download(board, fs::path(name, version, "data.txt"), progress = FALSE)
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
  version <- version_setup(board, name, metadata, versioned = versioned)

  version_dir <- fs::path(name, version)


  # Upload metadata
  local_azure_progress(FALSE)
  AzureStor::upload_blob(board$container,
    src = textConnection(yaml::as.yaml(metadata)),
    dest = fs::path(version_dir, "data.txt")
  )

  # Upload files
  local_azure_progress()
  keys <- fs::path(version_dir, fs::path_file(paths))
  AzureStor::multiupload_blob(board$container, src = paths, dest = keys)

  invisible(board)
}

# Helpers -----------------------------------------------------------------

azure_delete_dir <- function(board, dir) {
  ls <- AzureStor::list_blobs(board$container, dir)
  for (path in ls$name) {
    AzureStor::delete_blob(board$container, path, confirm = FALSE)
  }
}

azure_ls <- function(board, dir = "/") {
  ls <- AzureStor::list_blobs(
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
    AzureStor::multidownload_blob(board$container, keys[needed], paths[needed])
  }

  invisible()
}
