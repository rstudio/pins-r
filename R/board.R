new_board <- function(board, name, ...) {
  board <- structure(list(
      board = board,
      name = name
    ),
    class = board)

  board <- board_initialize(board, ...)

  board$info <- board_info(board)

  board
}

#' Connect to Board
#'
#' Connects to a board to activate RStudio's connection pane, when available.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @keywords internal
#' @export
board_connect <- function(name) {
  board <- board_get(name)

  ui_viewer_register(board)

  invisible(board)
}

#' List Boards
#'
#' Retrieves all available boards.
#'
#' @export
board_list <- function() {
  board_registry_list()
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

  board_registry_get(name)
}

#' Register Board
#'
#' Registers a board, useful to add sources to \code{pin_find()} or pin to remote
#' boards with \code{pin()}.
#'
#' @param board The name of the board to register.
#' @param name An optional name to identify this board, defaults to the board name.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
board_register <- function(board, name = board, ...) {
  params <- list(...)
  board <- new_board(board, name, ...)

  board_registry_set(name, board)

  if (identical(params$connect, TRUE)) board_connect(name)

  invisible(board)
}

#' Deregister Board
#'
#' Deregisters a board, useful to disable boards no longer in use.
#'
#' @param name An optional name to identify this board, defaults to the board name.
#'
#' @export
board_deregister <- function(name) {
  board_registry_set(name, NULL)
}

board_info <- function(board) {
  UseMethod("board_info")
}

board_info.default = function(board) {
  NULL
}
