.globals <- new.env(parent = emptyenv())

#' Use Board
#'
#' Defines which board to use, defaults to a board storing data
#' locally under a \code{~/pins} folder.
#'
#' @param name The name of the board to activate.
#' @param ... Additional parameters used in \code{"board_initialize"}.
#'
#' @name board
#' @export
use_board <- function(name, ...) {
  args <- list(...)

  class(args) <- name
  backend <- board_initialize(args, ...)
  backend$name <- name

  if (identical(.globals$backends, NULL)) .globals$backends <- list()

  .globals$backends[[name]] <- backend

  pins_viewer_register()
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
