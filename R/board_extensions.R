#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards extensions,
#' not to be used by users.
#'
#' @param board The board to extend, retrieved with `board_get()`.
#' @param path The path to store as a pin.
#' @param name The name of the pin.
#' @param metadata A list of metadata associated with this pin.
#' @param text The text patteren to find a pin.
#' @param ... Additional parameteres.
#'
#' @rdname custom-boards
#' @export
board_pin_create <- function(board, path, name, metadata, ...) {
  UseMethod("board_pin_create")
}

#' @export
#' @rdname custom-boards
board_initialize <- function(board, ...) {
  stop("`board_initialize()` is no longer used", call. = FALSE)
}

#' @export
#' @rdname custom-boards
board_pin_get <- function(board, name, ...) {
  UseMethod("board_pin_get")
}

#' @export
#' @rdname custom-boards
board_pin_remove <- function(board, name, ...) {
  UseMethod("board_pin_remove")
}

#' @export
#' @rdname custom-boards
board_pin_find <- function(board, text, ...) {
  UseMethod("board_pin_find")
}

#' @export
#' @rdname custom-boards
board_pin_versions <- function(board, name, ...) {
  UseMethod("board_pin_versions")
}

#' Custom Boards Utilities
#'
#' A set of utilities used when implementing custom boards.
#'
#' @export
#' @rdname custom-boards-utils
#' @keywords internal
board_local_storage <- function(...) {
  stop("board_local_storage() is deprecated", call. = FALSE)
}

#' @export
#' @rdname custom-boards
board_browse <- function(board, ...) {
  UseMethod("board_browse")
}

#' @export
board_browse.default <- function(board, ...) {
  invisible(NULL)
}

board_empty_results <- function() {
  data.frame(name = c(), description = c(), rows = c(), cols = c(), class = c())
}

#' @export
board_pin_versions.default <- function(board, name, ...) {
  data.frame(version = character(0), stringsAsFactors = FALSE)
}
