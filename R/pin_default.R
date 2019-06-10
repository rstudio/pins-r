#' @export
#' @keywords internal
pin.default <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) stop("The 'name' parameter is required for '", class(x)[[1]], "' objects.")

  board_create_pin(board_get(board), x, name, description, "default", "")
}

pin_preview_object.default <- function(x) {
  x
}
