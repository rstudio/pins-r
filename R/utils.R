get_function <- function(name, package) {
  if (length(find.package(package, quiet = TRUE)) == 0) {
    NULL
  }
  else {
    get0(name, envir = asNamespace(package))
  }
}

pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE) && interactive()
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

pins_safe_csv <- function(x, name) {
  tryCatch({
    pins_save_csv(x, name)
  }, error = function(e) {
    warning("Failed to save data frame as CSV file")
  })
}
