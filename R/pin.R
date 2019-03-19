#' Dataset Pin
#'
#' Pins the given dataset in the active board or retrieves
#' a named pin from the active board.
#'
#' @param x A dataset to pin or the named pin to retrieve.
#' @param name The name for the dataset.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @export
pin <- function(x = NULL, name = NULL, description = "", board = active_board(), ...) {
  if (is.null(name) && !is.null(x)) {
    name <- x
    result <- pin_retrieve(board, name)
  }
  else if (!is.null(name) && is.null(x)) {
    result <- pin_retrieve(board, name)
  }
  else {
    unpin(name, board = board)

    x <- pin_pack(x, ...)
    pin_create(board, x, name, description)

    pins_viewer_updated()

    result <- pin(name)
  }

  result <- pin_unpack(result, ...)

  attr(result, "pin_name") <- name

  pins_viewer_ensure(board)
  result
}

pin_pack <- function(x, ...) {
  UseMethod("pin_pack")
}

pin_unpack <- function(x, ...) {
  UseMethod("pin_unpack")
}

pin_pack.default <- function(x, ...) {
  x
}

pin_unpack.default <- function(x, ...) {
  x
}

pin_create <- function(board, x, name, description) {
  UseMethod("pin_create")
}

pin_retrieve <- function(board, name) {
  UseMethod("pin_retrieve")
}

#' Unpin Dataset
#'
#' Unpins the given named pin from the active board.
#'
#' @param name The name for the dataset.
#' @param board The board where this pin will be placed.
#'
#' @export
unpin <- function(name, board = active_board()) {
  pins_viewer_ensure(board)

  pin_remove(board, name)
}

pin_remove <- function(board, name) {
  UseMethod("pin_remove")
}

#' Find Pin
#'
#' Find a pin in the active board.
#'
#' @param name The name for the pin
#' @param board The board where this pin will be placed.
#'
#' @export
find_pin <- function(name = NULL, board = NULL) {
  if (is.null(board)) board <- all_boards()

  all_pins <- data.frame(name = character(), description = character())

  for (board_name in board) {
    board_object <- get_board(board_name)

    board_pins <- pin_find(board = board_object, name)
    all_pins <- rbind(all_pins, board_pins)
  }

  all_pins
}

pin_find <- function(board, name) {
  UseMethod("pin_find")
}
