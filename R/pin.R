#' Dataset Pin
#'
#' Pins the given dataset in the active board or retrieves
#' a named pin from the active board.
#'
#' @param x A dataset to pin or the named pin to retrieve.
#' @param name The name for the dataset.
#' @param description Optional description for this pin.
#' @param path Optional location where this dataset was stored.
#' @param board The board where this pin will be placed.
#'
#' @export
pin <- function(x = NULL, name = NULL, description = "", path = NULL, board = active_board()) {
  if (is.null(name) && !is.null(x)) {
    pin_retrieve(board, x)
  }
  else if (!is.null(name) && is.null(x)) {
    pin_retrieve(board, name)
  }
  else {
    unpin(name, board = board)

    pin_create(board, x, name, description)

    pins_viewer_updated()

    pin(name)
  }
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
    board_pins <- pin_find(board = get_board(board_name), name)
    all_pins <- rbind(all_pins, board_pins)
  }

  all_pins
}

pin_find <- function(board, name) {
  UseMethod("pin_find")
}
