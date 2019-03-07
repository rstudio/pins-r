.globals <- new.env(parent = emptyenv())

#' Use Board
#'
#' Defines which board to use, defaults to a board storing data
#' locally under a \code{~/pins} folder.
#'
#' @param ... A list of boards to use, defaults to \code{"local"}.
#'
#' @name board
#' @export
use_board <- function(...) {
  backends <- list(...)

  .globals$backends <- backends

  pins_viewer_register()
}

#' @name board
#' @export
active_board <- function() {
  board <- .globals$backends[[1]]
  if (is.null(board)) "local" else board
}
