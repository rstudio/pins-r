#' Register Local Board
#'
#' Wrapper with explicit parameters over `board_register()` to
#' register a local folder as a board.
#'
#' @inheritParams board_register
#' @param cache The folder where pins will be read/written.
#'
#' @seealso board_register
#'
#' @examples
#' # register local board using a temp folder
#' board_register_local(cache = tempfile())
#' @export
board_register_local <- function(name = "local",
                                 cache = board_cache_path(),
                                 ...) {

  board <- board_local(name = name, cache = cache, ...)
  board_register2(board)
}

#' @rdname board_register_local
#' @export
board_local <- function(cache = board_cache_path(),
                        name = "local",
                        versions = FALSE) {

  new_board("local", name = name, cache = cache, versions = versions)
}

#' @export
board_pin_create.local <- function(board, path, name, metadata, ...) {
  board_versions_create(board, name, path)

  # TODO: figure out how to handle names that are not valid paths
  cache_path <- fs::path(board$cache, board$name, name)
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
board_pin_find.local <- function(board, text, ...) {
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
board_pin_get.local <- function(board, name, version = NULL, ...) {
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
board_pin_remove.local <- function(board, name, ...) {
  pin_registry_remove(board, name)
}

#' @export
board_pin_versions.local <- function(board, name, ...) {
  board_versions_get(board, name)
}
