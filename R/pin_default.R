#' @keywords internal
#' @export
pin.default <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) stop("The 'name' parameter is required for '", class(x)[[1]], "' objects.")

  path <- tempfile()
  dir.create(path)

  saveRDS(x, file.path(path, "data.rds"), version = 2)
  on.exit(unlink(path))

  board_pin_store(board_get(board), path, name, description, "default", list(), ...)
}

#' @keywords internal
#' @export
pin_preview.default <- function(x, board = NULL, ...) {
  data.frame(
    files = x,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @export
pin_load.default <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)

  files[!grepl("data\\.txt$", files)]
}
