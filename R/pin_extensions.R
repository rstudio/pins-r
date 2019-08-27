#' Custom Pins
#'
#' Family of functions meant to be used to implement custom pin extensions, not to be used by users.
#'
#' @param board The board to extended, retrieved with \code{board_get()}.
#' @param name The name of the pin.
#' @param path The path to store.
#' @param description The text patteren to find a pin.
#' @param type The type of pin being stored.
#' @param metadata A list containing additional metadata desecribing the pin.
#' @param ... Additional parameteres.
#'
#' @export
#' @rdname custom-pins
board_pin_store <- function(board, path, name, description, type, metadata, ...) {
  board <- board_get(board)
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(path)))

  if (identical(list(...)$cache, FALSE)) pin_reset_cache(board$name, name)

  path <- path[!grepl("data\\.txt", path)]

  store_path <- tempfile()
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE))

  for (single_path in path) {
    if (grepl("^http", single_path)) {
      single_path <- pin_download(single_path, name, "local", ...)
    }

    if (dir.exists(single_path)) {
      file.copy(dir(single_path, full.names = TRUE) , store_path, recursive = TRUE)
    }
    else {
      file.copy(single_path, store_path, recursive = TRUE)
    }
  }

  metadata$description <- description
  metadata$type <- type

  pin_manifest_create(store_path, metadata, dir(store_path, recursive = TRUE))

  board_pin_create(board, store_path, name = name, metadata = metadata, ...)

  pin_get(name, board$name, ...)
}
