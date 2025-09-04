#' Delete a pin (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Deletes pins from a legacy board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @export
#' @keywords internal
pin_remove <- function(name, board = NULL) {
  lifecycle::deprecate_stop("1.4.0", "pin_remove()", "pin_delete()")
  board <- board_get(board)
  board_pin_remove(board, name)
  invisible(NULL)
}
