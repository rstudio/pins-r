#' @keywords internal
#' @export
pin.character <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  extension <- if (length(x) > 1) "zip" else tools::file_ext(x)
  board_pin_store(board, x, name, description, "files", list(extension = extension), ...)
}
