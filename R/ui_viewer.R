# nocov start

#' Preview a pin (legacy API)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' `pin_preview()` is no longer supported.
#'
#' @export
#' @keywords internal
pin_preview <- function(x, board = NULL, ...) {
  lifecycle::deprecate_stop("1.1.0", "pin_preview()")
}

# nocov end
