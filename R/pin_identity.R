#' @keywords internal
#' @export
pin.AsIs <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  pin.default(x = x, name = name, description = description, board = board, ...)
}
