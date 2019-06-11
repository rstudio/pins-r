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

#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards, not to be
#' used by end users.
#'
#' @param x A local file path or an object. Boards must support storing both.
#'
#' @rdname custom-boards
#' @export
#' @keywords internal
board_create_pin <- function(board, path, name, description, type, metadata) {
  UseMethod("board_create_pin")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_get <- function(board, name, details) {
  UseMethod("board_pin_get")
}

board_pin_get_or_null <- function(...) {
  tryCatch(board_pin_get(...), error = function(e) NULL)
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_remove_pin <- function(board, name) {
  UseMethod("board_remove_pin")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_find_pin <- function(board, text, ...) {
  UseMethod("board_find_pin")
}

#' Connect to Board
#'
#' Connects to a board to activate RStudio's connection pane, when available.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
board_connect <- function(name) {
  board <- board_get(name)

  ui_viewer_register(board)

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
#' @param connect Try open board in RStudio's connections pane?
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @export
board_register <- function(name, connect = TRUE, ...) {
  board <- new_board(name, ...)

  if (identical(.globals$boards_registered, NULL)) .globals$boards_registered <- list()
  .globals$boards_registered[[name]] <- board

  if (identical(connect, TRUE)) board_connect(name)

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
