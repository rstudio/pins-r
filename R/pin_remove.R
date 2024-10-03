#' Delete a pin (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Deletes pins from a legacy board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @examplesIf rlang::is_installed("filelock")
#' # old API
#' board_register_local(cache = tempfile())
#' pin(mtcars)
#' pin_remove("mtcars")
#'
#' # new API
#' board <- board_local()
#' board %>% pin_write(mtcars)
#' board %>% pin_delete("mtcars")
#' @export
#' @keywords internal
pin_remove <- function(name, board = NULL) {
  lifecycle::deprecate_soft("1.4.0", "pin_remove()", "pin_delete()")
  board <- board_get(board)
  board_pin_remove(board, name)
  invisible(NULL)
}
