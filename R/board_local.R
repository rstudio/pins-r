#' Use a local folder as board
#'
#' @description
#' * `board_folder()` creates a board inside a folder. You can use this to
#'    share files by using a folder on a shared network drive or inside
#'    a DropBox.
#'
#' * `board_temp()` creates a temporary board that lives in a session
#'    specific temporary directory. It will be automatically deleted once
#'    the current R session ends.
#'
#' * `board_local()` is mostly included for backward compatiblity. It's
#'    the default board used by [pin()] and [pin_get()].
#'
#' @inheritParams new_board
#' @param path Path to directory to store pins. Will be created if it
#'   doesn't already exist.
#' @family boards
#' @examples
#' # session-specific local board
#' board <- board_temp()
#' @export
board_folder <- function(path, name = "folder", versions = FALSE) {
  new_board("pins_board_local",
    name = name,
    cache = path,
    versions = versions
  )
}

#' @export
#' @rdname board_folder
board_local <- function(versions = FALSE) {
  board_folder(board_cache_path("local"), name = "local", versions = versions)
}

#' @rdname board_folder
#' @export
board_temp <- function(name = "temp", versions = FALSE) {
  board_folder(fs::file_temp("pins-"), name = name, versions = versions)
}

#' @export
board_desc.pins_board_local <- function(board, ...) {
  paste0("Path: '", board$cache, "'")
}

#' @export
board_browse.pins_board_local <- function(board, ...) {
  utils::browseURL(board$cache)
}

#' @export
board_pin_create.pins_board_local <- function(board, path, name, metadata, ...) {
  board_versions_create(board, name, path)

  # TODO: figure out how to handle names that are not valid paths
  cache_path <- pin_registry_path(board, name)

  if (fs::dir_exists(cache_path)) {
    delete <- fs::dir_ls(cache_path)
    delete <- delete[!grepl("(/|\\\\)_versions$", delete)]
    unlink(delete, recursive = TRUE)
  } else {
    fs::dir_create(cache_path)
  }

  file.copy(fs::dir_ls(path), cache_path, recursive = TRUE)

  metadata$path <- name
  pin_registry_update(board, name, metadata)
}

#' @export
board_pin_find.pins_board_local <- function(board, text, ...) {
  results <- pin_registry_find(board, text)

  # if (nrow(results) == 1) {
  #   metadata <- jsonlite::fromJSON(results$metadata)
  #   path <-  pin_registry_absolute(metadata$path, component = board$name)
  #   extended <- pin_manifest_get(path)
  #   merged <- pin_manifest_merge(metadata, extended)
  #
  #   results$metadata <- as.character(jsonlite::toJSON(merged, auto_unbox = TRUE))
  # }

  results
}

#' @export
board_pin_get.pins_board_local <- function(board, name, version = NULL, ...) {
  rel_path <- pin_registry_retrieve(board, name)$path
  path <- pin_registry_path(board, rel_path)

  if (!is.null(version)) {
    manifest <- pin_manifest_get(path)
    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    path <- fs::path(path, version)
  }

  path
}

#' @export
board_pin_remove.pins_board_local <- function(board, name, ...) {
  pin_registry_remove(board, name)
}

#' @export
board_pin_versions.pins_board_local <- function(board, name, ...) {
  board_versions_get(board, name)
}
