#' Create Pin
#'
#' Pins the given dataset or object in the active board.
#'
#' @param x The dataset or object to pin.
#' @param name The name for the dataset or object.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @export
pin <- function(x, name, description = "", board = active_board(), ...) {
  unpin(name, board = board)

  x <- pin_pack(x, board, ...)
  pin_create(board, x, name, description)

  pins_viewer_updated()

  result <- get_pin(name)

  result <- pin_unpack(result, board, ...)

  attr(result, "pin_name") <- name

  pins_viewer_ensure(board)
  result
}

#' Retrieve Pin
#'
#' Retrieves a named pin from the active board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @export
get_pin <- function(name, board = active_board(), ...) {
  result <- pin_retrieve(board, name)

  result <- pin_unpack(result, board, ...)

  attr(result, "pin_name") <- name

  result
}

pin_pack <- function(x, board, ...) {
  UseMethod("pin_pack")
}

pin_unpack <- function(x, board, ...) {
  UseMethod("pin_unpack")
}

pin_pack.default <- function(x, board, ...) {
  x
}

pin_unpack.default <- function(x, board, ...) {
  x
}

pin_create <- function(board, x, name, description) {
  UseMethod("pin_create")
}

pin_retrieve <- function(board, name) {
  UseMethod("pin_retrieve")
}

#' Remove Pin
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

  invisible(name)
}

pin_remove <- function(board, name) {
  UseMethod("pin_remove")
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{use_board()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#'
#' @export
find_pin <- function(text = NULL, board = NULL) {
  if (is.null(board)) board <- all_boards()

  all_pins <- data.frame(name = character(), description = character(), board = character())

  for (board_name in board) {
    board_object <- get_board(board_name)

    board_pins <- pin_find(board = board_object, text)
    board_pins$board <- rep(board_name, nrow(board_pins))
    all_pins <- rbind(all_pins, board_pins)
  }

  if (!is.null(text)) {
    find_names <- grepl(text, all_pins$name)
    find_description <- grepl(text, all_pins$description)
    all_pins <- all_pins[find_names | find_description,]
  }

  maybe_tibble(all_pins)
}

pin_find <- function(board, text) {
  UseMethod("pin_find")
}
