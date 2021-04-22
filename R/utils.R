http_utils_progress <- function(type = "down", size = 0) {
  if (pins_show_progress(size = size)) {
    httr::progress(type = type)
  } else {
    NULL
  }
}

pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE) && interactive()
}

has_envvars <- function(x) {
  all(Sys.getenv(x) != "")
}

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

modifyList <- function(x, y) {
  if (is.null(x)) {
    y
  } else if (is.null(y)) {
    x
  } else {
    utils::modifyList(x, y)
  }
}

last <- function(x) x[[length(x)]]

pins_inform <- function(...) {
  opt <- getOption("pins.quiet", NA)

  if (identical(opt, FALSE) || (identical(opt, NA) && !is_testing())) {
    inform(...)
  }
}
ui_quiet <- function() {
  withr::local_options("pins.quiet" = TRUE, .local_envir = parent.frame())
}
ui_loud <- function() {
  withr::local_options("pins.quiet" = FALSE, .local_envir = parent.frame())
}
