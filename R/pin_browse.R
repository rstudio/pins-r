#' Browse source of a pin
#'
#' `pin_browse()` navigates you to the home of a pin, either on the
#' internet or on your local file system.
#'
#' @inheritParams pin_read
#' @param ... Other arguments passed on to methods.
#' @param cache If `TRUE`, will open the directory on your computer used
#'   to cache pin files.
#' @export
pin_browse <- function(board, name, version = NULL, ..., cache = FALSE) {
  ellipsis::check_dots_used()
  UseMethod("pin_browse")
}
