#' Browse source of a pin
#'
#' `pin_browse()` navigates you to the home of a pin, either on the
#' internet or on your local file system.
#'
#' @inheritParams pin_read
#' @param local If `TRUE`, will open the local copy of the pin; otherwise
#'   will show you the home of the pin on the internet.
#' @export
#' @examples
#' board <- board_temp(versioned = TRUE)
#' board %>% pin_write(1:10, "x")
#' board %>% pin_write(1:11, "x")
#' board %>% pin_write(1:12, "x")
#'
#' board %>% pin_browse("x", local = TRUE)
pin_browse <- function(board, name, version = NULL, local = FALSE) {
  meta <- pin_meta(board, name, version = version)

  if (local) {
    url <- meta$local$dir
    if (is.null(url)) {
      abort("pin doesn't have local cache")
    }
  } else {
    url <- meta$local$url
    if (is.null(url)) {
      abort("pin doesn't have remote url")
    }
  }
  browse_url(url)
}

browse_url <- function(x) {
  if (is_interactive()) {
    utils::browseURL(x)
  } else {
    cli::cli_alert_info("Pin at {.url {x}}")
  }
}
