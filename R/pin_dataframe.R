#' @keywords internal
#' @export
pin.data.frame <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) name <- pin_default_name(deparse(substitute(x)), board)

  path <- tempfile()
  dir.create(path)

  saveRDS(x, file.path(path, "data.rds"), version = 2)
  write.csv(x, file.path(path, "data.csv"), row.names = FALSE)
  on.exit(unlink(path))

  columns <- lapply(x, function(e) class(e)[[1]])
  names(columns) <- names(x)

  metadata <- list(
    rows = nrow(x),
    cols = ncol(x),
    columns = columns
  )

  board_pin_store(board_get(board), path, name, description, "table", metadata,...)
}

#' @keywords internal
#' @export
pin_load.table <- function(path, ...) {
  readRDS(file.path(path, "data.rds"))
}

#' @keywords internal
#' @export
pin_preview.data.frame <- function(x, board = NULL, ...) {
  utils::head(x, n = getOption("pins.preview", 10^3))
}
