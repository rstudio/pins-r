http_utils_progress <- function(type = "down", size = 0) {
  if (pins_show_progress(size = size))
    httr::progress(type = type)
  else
    NULL
}

pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE) && interactive()
}

has_envvars <- function(x) {
  all(Sys.getenv(x) != "")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
