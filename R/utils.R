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

is_url <- function(x) {
  grepl("^http://|^https://", x)
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Pin Logging
#'
#' Log message for diagnosing the `pins` package.
#'
#' @param ... Entries to be logged.
#'
#' @export
#' @keywords internal
pin_log <- function(...) {
  if (getOption("pins.verbose", FALSE) && !is_testing()) {
    message(...)
  }
}

format_tibble <- function(data) {
  if (!is.data.frame(data)) return(data)

  if (length(find.package("tibble", quiet = TRUE)) > 0 && !identical(getOption("pins.tibble"), FALSE)) {
    tibble::as_tibble(data)
  }
  else {
    data
  }
}
