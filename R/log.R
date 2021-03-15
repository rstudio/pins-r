#' Pin Logging
#'
#' Log message for diagnosing the `pins` package.
#'
#' @param ... Entries to be logged.
#'
#' @export
#' @keywords internal
pin_log <- function(...) {
  if (getOption("pins.verbose", FALSE) && !is_testing()) {
    message(...)
  }
}
