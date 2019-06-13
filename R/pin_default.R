#' @keywords internal
#' @export
pin.default <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) stop("The 'name' parameter is required for '", class(x)[[1]], "' objects.")

  path <- tempfile(fileext = ".rds")
  saveRDS(x, path, version = 2)
  on.exit(unlink(path))

  board_pin_store(board_get(board), path, name, description, "default", list())
}

#' @keywords internal
#' @export
pin_preview.default <- function(x) {
  x
}

#' @keywords internal
#' @export
pin_load.default <- function(path) {
  path
}
