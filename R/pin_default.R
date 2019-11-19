#' @keywords internal
#' @export
pin.default <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) name <- pin_default_name(deparse(substitute(x)), board)

  path <- tempfile()
  dir.create(path)

  saveRDS(x, file.path(path, "data.rds"), version = 2)
  on.exit(unlink(path))

  board_pin_store(board, path, name, description, "default", list(), ...)
}

#' @keywords internal
#' @export
pin_preview.default <- function(x, board = NULL, ...) {
  x
}

#' @keywords internal
#' @export
pin_load.default <- function(path, ...) {
  result <- readRDS(file.path(path, "data.rds"))

  if ("AsIs" %in% class(result)) {
    class(result) <- class(result)[class(result) != "AsIs"]
  }

  result
}

#' @keywords internal
#' @export
pin_fetch.default <- function(path, ...) {
  path
}
