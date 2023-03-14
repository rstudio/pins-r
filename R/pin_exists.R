#' Determine if a pin exists
#'
#' @inheritParams pin_read
#' @export
pin_exists <- function(board, name, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_exists")
}

check_pin_exists <- function(board, name, call = caller_env()) {
  if (pin_exists(board, name)) {
    invisible()
  } else {
    abort_pin_missing(name, call = call)
  }
}
