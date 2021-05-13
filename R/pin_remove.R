#' Remove Pin
#'
#' Unpins the given named pin from the given board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @examples
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
  board <- board_get(board)

  board_pin_remove(board, name)
  ui_viewer_updated(board)

  invisible(NULL)
}
