#' Determine required packages for a pins board
#'
#' @param x A pin board
#' @param ... Not used.
#' @return A character vector of package names required to use the board.
#' @name required_pkgs.pins_board
#' @keywords internal
#' @examples
#' required_pkgs(board_temp())
#'
#' @export
required_pkgs.pins_board <- function(x, ...) {
  check_dots_empty()
  character(0)
}
