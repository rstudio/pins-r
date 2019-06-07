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

board_pin_get <- function(board, name, details) {
  UseMethod("board_pin_get")
}

board_pin_get_or_null <- function(...) {
  tryCatch(board_pin_get(...), error = function(e) NULL)
}

#' Connect to Board
#'
#' Connects to a board to activate RStudio's conneection pane, when available.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
board_connect <- function(name, ...) {
  board_call <- paste("library(pins)", deparse(match.call(), width.cutoff = 500), sep = "\n")

  board <- new_board(name, ...)

  pins_viewer_register(board, board_call)

  invisible(board)
}

board_initialize <- function(name, ...) {
  UseMethod("board_initialize")
}

#' List Boards
#'
#' Retrieves all available boards.
#'
#' @export
board_list <- function() {
  names(.globals$boards_registered)
}

#' Get Board
#'
#' Retrieves information about a particular board.
#'
#' @param name The name of the board to use
#'
#' @export
board_get <- function(name = NULL) {
  if (is.null(name)) name <- getOption("pins.board", "local")

  if (!name %in% board_list())
    stop("Board '", name, "' not a board, available boards: ", paste(board_list(), collapse = ", "))

  .globals$boards_registered[[name]]
}

#' Register Board
#'
#' Registers a board, useful to add sources to \code{pin_find()}.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
board_register <- function(name, ...) {
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
