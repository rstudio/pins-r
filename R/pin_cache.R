#' Cache a pin locally
#'
#' This is an internal function, needed primarily for folks who are developing
#' new board types.
#'
#' @return Pin metadata with two extra fields `cache_dir` and `cache_paths`
#' @export
#' @keywords internal
pin_cache <- function(board, name, version = NULL, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_cache")
}
