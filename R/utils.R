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
    inform(glue(..., .envir = caller_env()))
  }
}

ui_quiet <- function() {
  withr::local_options("pins.quiet" = TRUE, .local_envir = parent.frame())
}
ui_loud <- function() {
  withr::local_options("pins.quiet" = FALSE, .local_envir = parent.frame())
}

github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)

write_yaml <- function(x, path) {
  x <- to_utf8(x)
  yaml::write_yaml(x, path)
}

# On Windows, yaml::write_yaml() crashes with Latin1 data
# https://github.com/viking/r-yaml/issues/90
to_utf8 <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      names(x) <- enc2utf8(names(x))
    }
    lapply(x, to_utf8)
  } else if (is.character(x)) {
    enc2utf8(x)
  } else {
    x
  }
}

envvar_get <- function(name) {
  null_if_na(Sys.getenv(name, NA))
}

this_not_that <- function(this, that) {
  abort(glue("Use `{this}` with this board, not `{that}`"))
}

check_board_deparse <- function(board, arg) {
  if (has_name(board, arg)) {
    return(board[[arg]])
  } else {
    abort(glue("No {arg} found for this board"))
  }
}

null_if_na <- function(x) {
  if (length(x) == 1 && is.na(x)) {
    NULL
  } else {
    x
  }
}
