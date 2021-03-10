#' @keywords internal
#' @export
pin.data.frame <- function(x, name = NULL, description = NULL, board = NULL, ...) {

  # Used to avoid mutation in pins_save_csv
  if ("data.table" %in% class(x)) {
    return(
      pin.default(x, name = name, description = description, board = board, ...)
    )
  }

  if (is.null(name)) name <- pin_default_name(deparse(substitute(x)), board)

  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path))

  saveRDS(x, file.path(path, "data.rds"), version = 2)
  pins_safe_csv(x, file.path(path, "data.csv"))

  metadata <- list(
    rows = nrow(x),
    cols = ncol(x),
    columns = lapply(x, function(e) class(e)[[1]])
  )
  board_pin_store(board, path, name, description, "table", metadata,...)
}

pins_safe_csv <- function(x, name) {
  tryCatch({
    pins_save_csv(x, name)
  }, error = function(e) {
    warning("Failed to save data frame as CSV file")
  })
}

pins_save_csv <- function(x, name) {
  supported_columns <- c(
    "character",
    "numeric",
    "integer",
    "Date",
    "POSIXlt",
    "logical",
    "raw"
  )

  x_class <- unname(sapply(x, function(e) class(e)[[1]]))
  unsupported_columns <- which(!x_class %in% supported_columns)
  for (col_idx in unsupported_columns) {
    x[[col_idx]] <- as.character(x[[col_idx]])
  }

  utils::write.csv(x, name, row.names = FALSE)
}

#' @keywords internal
#' @export
pin_load.table <- function(path, ...) {
  rds <- file.path(path, "data.rds")
  csv <- file.path(path, "data.csv")

  if (file.exists(rds)) result <- readRDS(rds)
  else if (file.exists(csv)) result <- utils::read.csv(csv, stringsAsFactors = FALSE)
  else stop("A 'table' pin requires CSV or RDS files.")

  format_tibble(result)
}

#' @keywords internal
#' @export
pin_fetch.table <- function(path, ...) {
  rds_match <- grepl(".*.rds", path)
  fetch_all <- identical(getOption("pins.fetch", "auto"), "all")
  if (any(rds_match) && !fetch_all) path[rds_match] else path
}

#' @keywords internal
#' @export
pin_preview.data.frame <- function(x, board = NULL, ...) {
  utils::head(x, n = getOption("pins.preview", 10^3))
}
