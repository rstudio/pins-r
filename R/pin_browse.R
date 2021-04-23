#' Browse source of a pin
#'
#' `pin_browse()` navigates you to the home of a pin, either on the
#' internet or on your local file system.
#'
#' @inheritParams pin_read
#' @param ... Other arguments passed on to methods.
#' @param cache If `TRUE`, will open the directory on your computer used
#'   to cache pin files.
#' @export
#' @examples
#' board <- board_temp(versions = TRUE)
#' board %>% pin_write(1:10, "x")
#' board %>% pin_write(1:11, "x")
#' board %>% pin_write(1:12, "x")
#'
#' board %>% pin_browse("x")
pin_browse <- function(board, name, version = NULL, ..., cache = FALSE) {
  ellipsis::check_dots_used()
  UseMethod("pin_browse")
}

browse_url <- function(x) {
  if (is_interactive()) {
    utils::browseURL(x)
  } else {
    cli::cli_alert_info("Pin at {.url {x}}")
  }
}
