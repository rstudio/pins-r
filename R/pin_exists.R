#' Does a pin exist in a board?
#'
#' @inheritParams pin_read
#' @export
pin_exists <- function(board, name, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_exists")
}

check_pin_exists <- function(board, name) {
  if (pin_exists(board, name)) {
    invisible()
  } else {
    abort(c(
      glue("Can't find pin called '{name}'"),
      i = "Use `pin_list()` to see all available pins in this board"
    ), class = "pins_pin_absent")
  }
}
