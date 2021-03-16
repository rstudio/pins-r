
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
#' # define temporary board
#' board <- board_local(tempfile())
#'
#' # create mtcars pin
#' pin(mtcars, board = board)
#'
#' # remove mtcars pin
#' pin_remove("mtcars", board = board)
#' @export
pin_remove <- function(name, board) {
  board <- board_get(board)

  board_pin_remove(board, name)
  ui_viewer_updated(board)

  invisible(NULL)
}
