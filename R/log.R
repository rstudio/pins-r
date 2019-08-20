#' Pin Logging
#'
#' Log message for diagnosing the \code{pins} package.
#'
#' @param ... Entries to be logged.
#'
#' @export
#' @keywords internal
pin_log <- function(...) {
  if (getOption("pins.verbose", FALSE)) {
    message(...)
  }
}
