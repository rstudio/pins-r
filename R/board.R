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

  invisible(name)
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
    get_board("local")
}

#' @name board
#' @export
all_boards <- function() {
  if (is.null(.globals$boards))
    c(
      "packages",
      "local"
    )
  else
    c(
      names(.globals$boards),
      names(.globals$boards_registered)
    )
}

get_board <- function(name) {
  if (!name %in% all_boards())
    stop("Board 'name' not a board, available boards: ", paste(all_boards(), collapse = ", "))

  if (identical(as.character(name), "local"))
    structure(list(
      name = "local"
    ), class = "local")
  else if (name %in% names(.globals$boards))
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
#' @keywords internal
#' @export
register_board <- function(name, ...) {
  class(name) <- name

  board <- board_initialize(name, ...)
  board$name <- as.character(name)
  class(board) <- board$name

  if (identical(.globals$boards_registered, NULL)) .globals$boards_registered <- list()

  .globals$boards_registered[[name]] <- board

  invisible(name)
}
