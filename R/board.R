.globals <- new.env(parent = emptyenv())

new_board <- function(name, ...) {
  board <- structure(list(
    name = name
  ),
  class = name)

  board <- board_initialize(board, ...)

  board$info <- board_info(board)

  board
}

#' Use Board
#'
#' Defines which board to use, defaults to a board storing data
#' locally under a \code{~/pins} folder.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
use_board <- function(name, ...) {
  board_call <- deparse(match.call(), width.cutoff = 500)

  board <- new_board(name, ...)

  if (identical(.globals$boards, NULL)) .globals$boards <- list()

  .globals$boards[[name]] <- board

  pins_viewer_register(board, board_call)

  invisible(name)
}

board_initialize <- function(name, ...) {
  UseMethod("board_initialize")
}

active_board <- function() {
  if (length(.globals$boards) > 0)
    .globals$boards[[length(.globals$boards)]]
  else
    get_board("local")
}

#' All Boards
#'
#' Retrieves all available boards.
#'
#' @export
all_boards <- function() {
  unique(
    c(
      names(.globals$boards),
      names(.globals$boards_registered)
    )
  )
}

#' Get Board
#'
#' Retrieves information about a particular board.
#'
#' @param name The name of the board to use
#'
#' @export
get_board <- function(name = NULL) {
  if (is.null(name))
    return(active_board())

  if (!name %in% all_boards())
    stop("Board 'name' not a board, available boards: ", paste(all_boards(), collapse = ", "))

  if (name %in% names(.globals$boards))
    .globals$boards[[name]]
  else
    .globals$boards_registered[[name]]
}

#' Register Board
#'
#' Registers a board without making it active, useful to add sources to
#' \code{find_pin()}. This function is meant to be used while building extension
#' not by users directly.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
register_board <- function(name, ...) {
  board <- new_board(name, ...)

  if (identical(.globals$boards_registered, NULL)) .globals$boards_registered <- list()

  .globals$boards_registered[[name]] <- board

  invisible(name)
}

board_info <- function(board) {
  UseMethod("board_info")
}

board_info.default = function(board) {
  list(
    install_html = ""
  )
}
