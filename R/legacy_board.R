
#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards extensions,
#' not to be used by users.
#'
#' @param board The board to extend, retrieved with `board_get()`.
#' @param path The path to store as a pin.
#' @param name The name of the pin.
#' @param metadata A list of metadata associated with this pin.
#' @param text The text pattern to find a pin.
#' @param ... Additional parameters.
#'
#' @rdname custom-boards
#' @keywords internal
#' @export
board_pin_create <- function(board, path, name, metadata, ...) {
  UseMethod("board_pin_create")
}

#' @export
board_pin_create.pins_board <- function(board, path, name, metadata, ...) {
  this_not_that("pin_write", "pin")
}

#' @export
#' @rdname custom-boards
board_initialize <- function(board, ...) {
  stop("`board_initialize()` is no longer used", call. = FALSE)
}

#' @export
#' @rdname custom-boards
board_browse <- function(board, ...) {
  UseMethod("board_browse")
}

#' @export
board_browse.pins_board <- function(board, ...) {
  this_not_that("pin_browse", "board_browse")
}

#' @export
#' @rdname custom-boards
board_desc <- function(board, ...) {
  UseMethod("board_desc")
}
#' @export
board_desc.default <- function(board, ...) {
  character()
}

#' @export
#' @rdname custom-boards
board_pin_get <- function(board, name, ...) {
  UseMethod("board_pin_get")
}

#' @export
board_pin_get.pins_board <- function(board, name, ...) {
  this_not_that("pin_read", "pin_get")
}

#' @export
#' @rdname custom-boards
board_pin_remove <- function(board, name, ...) {
  UseMethod("board_pin_remove")
}

#' @export
board_pin_remove.pins_board <- function(board, name, ...) {
  this_not_that("pin_delete", "pin_remove")
}

#' @export
#' @rdname custom-boards
board_pin_find <- function(board, text, ...) {
  UseMethod("board_pin_find")
}

#' @export
board_pin_find.pins_board <- function(board, text, ...) {
  this_not_that("pin_search", "pin_find")
}

#' @export
#' @rdname custom-boards
board_pin_versions <- function(board, name, ...) {
  UseMethod("board_pin_versions")
}
#' @export
board_pin_versions.default <- function(board, name, ...) {
  tibble::tibble(version = character(0))
}
