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

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  new_board("local", name = name, cache = cache, versions = versions)
}

#' @export
board_pin_create.local <- function(board, path, name, metadata, ...) {
  board_versions_create(board, name = name, path = path)

  final_path <- pin_storage_path(component = board$name, name = name)

  delete <- dir(final_path, full.names = TRUE)
  delete <- delete[!grepl("(/|\\\\)_versions$", delete)]
  unlink(delete, recursive = TRUE)
  if (!dir.exists(final_path)) dir.create(final_path)

  file.copy(dir(path, full.names = TRUE) , final_path, recursive = TRUE)

  # reduce index size
  metadata$columns <- NULL

  base_path <- board_local_storage(board$name)

  pin_registry_update(
    name = name,
    params = c(list(
      path = pin_registry_relative(final_path, base_path = base_path)
    ), metadata),
    component = board$name)
}

#' @export
board_pin_find.local <- function(board, text, ...) {
  results <- pin_registry_find(text, board$name)

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    path <- pin_registry_absolute(metadata$path, component = board$name)
    extended <- pin_manifest_get(path)
    merged <- pin_manifest_merge(metadata, extended)

    results$metadata <- as.character(jsonlite::toJSON(merged, auto_unbox = TRUE))
  }

  results
}

#' @export
board_pin_get.local <- function(board, name, version = NULL, ...) {
  path <- pin_registry_retrieve_path(name, board$name)

  if (!is.null(version)) {
    manifest <- pin_manifest_get(pin_registry_absolute(path, board$name))

    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    path <- file.path(name, version)
  }

  pin_registry_absolute(path, component = board$name)
}

#' @export
board_pin_remove.local <- function(board, name, ...) {
  pin_registry_remove(name, board$name)
}

#' @export
board_pin_versions.local <- function(board, name, ...) {
  board_versions_get(board, name)
}
