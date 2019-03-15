.globals <- new.env(parent = emptyenv())

#' Use Board
#'
#' Defines which board to use, defaults to a board storing data
#' locally under a \code{~/pins} folder.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @name board
#' @export
use_board <- function(name, ...) {
  board_call <- deparse(match.call(), width.cutoff = 500)

  class(name) <- name

  board <- board_initialize(name, ...)
  board$name <- as.character(name)
  class(board) <- board$name

  if (identical(.globals$boards, NULL)) .globals$boards <- list()

  .globals$boards[[name]] <- board

  pins_viewer_register(board, board_call)
}

board_initialize <- function(name, ...) {
  UseMethod("board_initialize")
}

#' @name board
#' @export
active_board <- function() {
  if (length(.globals$boards) > 0)
    .globals$boards[[length(.globals$boards)]]
  else
    structure(list(
      name = "local"
    ), class = "local")
}

#' @name board
#' @export
all_boards <- function() {
  names(.globals$boards)
}
