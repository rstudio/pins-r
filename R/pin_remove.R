
#' Remove Pin
#'
#' Unpins the given named pin from the given board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @details
#'
#' Notice that some boards do not support deleting pins, this is the case
#' for the Kaggle board. For these boards, you would manually have to
#' remote resources using the tools the board provides.
#'
#' @examples
#'
#' library(pins)
#'
#' # define local board
#' board_register_local(cache = tempfile())
#'
#' # create mtcars pin
#' pin(mtcars)
#'
#' # remove mtcars pin
#' pin_remove("mtcars", board = "local")
#' @export
pin_remove <- function(name, board) {
  board <- board_get(board)

  board_pin_remove(board, name)
  ui_viewer_updated(board)

  invisible(NULL)
}
