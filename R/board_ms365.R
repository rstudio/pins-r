#' Use a OneDrive or Sharepoint document library as a board
#'
#' Pin data to a folder in Onedrive or a SharePoint Online document library
#' using the Microsoft365R package.
#'
#' @inheritParams new_board
#' @param drive A OneDrive or SharePoint document library object.
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
board_ms365 <- function(drive, path, versioned = TRUE, cache = NULL) {
  check_installed("Microsoft365R")

  try(drive$create_folder(path), silent=TRUE)
  folder <- drive$get_item(path)
  if (!folder$is_folder()) {
    abort("Invalid path specified")
  }

  cache <- cache %||% board_cache_path(paste0("ms365-", hash(folder$properties$id)))
  new_board_v1("pins_board_ms365",
    folder = folder,
    path = path,
    cache = cache,
    versioned = versioned
  )
}

#' @export
#' @rdname board_ms365
board_sharepoint <- board_ms365

#' @export
#' @rdname board_ms365
board_onedrive <- board_ms365

board_ms365_test <- function(...) {
  skip_if_missing_envvars("board_ms365()", "PINS_MS365_TEST")

  token <- readRDS(openssl::base64_decode(Sys.getenv("PINS_MS365_TEST")))
  lib <- AzureGraph::ms_graph$new(token=token)$get_user()$get_drive()

  board_ms365(lib, path = "pin_testing", cache = tempfile(), ...)
}

#' @export
pin_list.pins_board_ms365 <- function(board, ...) {
  ms365_list_dirs(board)
}

#' @export
pin_exists.pins_board_ms365 <- function(board, name, ...) {
  name %in% ms365_list_dirs(board)
}

#' @export
pin_delete.pins_board_ms365 <- function(board, names, by_item=FALSE, ...) {
  for (name in names) {
    check_pin_exists(board, name)
    ms365_delete_dir(board, name, by_item=by_item)
  }
  invisible(board)
}

#' @export
pin_versions.pins_board_ms365 <- function(board, name, ...) {
  check_pin_exists(board, name)
  version_from_path(ms365_list_dirs(board, name))
}

#' @export
pin_version_delete.pins_board_ms365 <- function(board, name, version, ...) {
  ms365_delete_dir(board, fs::path(name, version))
}

#' @export
pin_meta.pins_board_ms365 <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  version <- check_pin_version(board, name, version)
  metadata_key <- fs::path(name, version, "data.txt")

  if (!ms365_file_exists(board, metadata_key)) {
    abort_pin_version_missing(version)
  }

  path_version <- fs::path(board$cache, name, version)
  fs::dir_create(path_version)

  ms365_download(board, metadata_key)
  local_meta(
    read_meta(fs::path(board$cache, name, version)),
    dir = path_version,
    version = version
  )
}

#' @export
pin_fetch.pins_board_ms365 <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  for (file in meta$file) {
    key <- fs::path(name, meta$local$version, file)
    ms365_download(board, key)
  }

  meta
}

#' @export
pin_store.pins_board_ms365 <- function(board, name, paths, metadata,
                                    versioned = NULL, ...) {
  check_name(name)
  version <- version_setup(board, name, version_name(metadata), versioned = versioned)

  version_dir <- fs::path(name, version)

  # Upload metadata
  meta_tmpfile <- tempfile(fileext=".yml")
  on.exit(unlink(meta_tmpfile))
  yaml::write_yaml(metadata, meta_tmpfile)
  board$folder$upload(meta_tmpfile, fs::path(version_dir, "data.txt"))

  # Upload files
  for (path in paths) {
    board$folder$upload(path, fs::path(version_dir, fs::path_file(path)))
  }

  name
}


# helpers

# list all the directories inside 'path', which is assumed to live in the board folder
ms365_list_dirs <- function(board, path="") {
  conts <- board$folder$list_files(path)
  conts$name[conts$isdir]
}

# delete directory 'path', which is assumed to live in the board folder
ms365_delete_dir <- function(board, path="", by_item=FALSE) {
  child <- board$folder$get_item(path)
  child$delete(confirm=FALSE, by_item=by_item)
}

# check if a file exists and is not a directory
ms365_file_exists <- function(board, key) {
  item <- try(board$folder$get_item(key), silent=TRUE)
  inherits(item, "ms_drive_item") && !item$is_folder()
}


# download a specific file from the board, as given by the 'key' path
ms365_download <- function(board, key) {
  path <- fs::path(board$cache, key)

  if (!fs::file_exists(path)) {
    board$folder$get_item(key)$download(dest=path)
    fs::file_chmod(path, "u=r")
  }

  path
}
