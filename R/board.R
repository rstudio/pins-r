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
  class(name) <- name

  backend <- board_initialize(name, ...)
  backend$name <- name
  class(backend) <- as.character(name)

  if (identical(.globals$backends, NULL)) .globals$backends <- list()

  .globals$backends[[name]] <- backend

  pins_viewer_register()
}

board_initialize <- function(name, ...) {
  UseMethod("board_initialize")
}

#' @name board
#' @export
active_board <- function() {
  if (length(.globals$backends) > 0)
    .globals$backends[[length(.globals$backends)]]
  else
    structure(list(
      name = "local"
    ), class = "local")
}
