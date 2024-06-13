#' List all pins
#'
#' List names of all pins in a board. This is a low-level function; use
#' [pin_search()] to get more data about each pin in a convenient form.
#'
#' @inheritParams pin_write
#' @param ... Other arguments passed on to methods
#' @return A character vector
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_write(1:5, "x")
#' board %>% pin_write(letters, "y")
#' board %>% pin_write(runif(20), "z")
#'
#' board %>% pin_list()
pin_list <- function(board, ...) {
  check_dots_used()
  UseMethod("pin_list")
}
