#' @keywords internal
#' @export
pin.data.frame <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) stop("The 'name' parameter is required for '", class(x)[[1]], "' objects.")

  path <- tempfile(fileext = ".rds")
  saveRDS(x, path, version = 2)
  on.exit(unlink(path))

  metadata <- list(
    rows = nrow(x),
    cols = ncol(x)
  )

  board_pin_store(board_get(board), path, name, description, "table", metadata)
}

#' @keywords internal
#' @export
pin_load.table <- function(path, ...) {
  readRDS(path)
}

#' @keywords internal
#' @export
pin_preview.data.frame <- function(x, board = NULL, ...) {
  head(x, n = getOption("pins.preview", 10^3))
}
