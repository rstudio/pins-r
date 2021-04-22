#' Remove Pin
#'
#' Unpins the given named pin from the given board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @examples
#' board <- board_temp()
#' pin(mtcars, board = board)
#' pin_remove("mtcars", board = board)
#'
#' # ->
#' board %>% pin_write(mtcars)
#' board %>% pin_delete("mtcars")
#' @export
#' @keywords internal
pin_remove <- function(name, board) {
  board <- board_get(board)

  board_pin_remove(board, name)
  ui_viewer_updated(board)

  invisible(NULL)
}
