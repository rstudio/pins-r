#' Retrieve metadata for a pin
#'
#' @inheritParams pin_read
#' @returns A list.
#' @export
#' @examples
#' b <- board_temp()
#' b %>% pin_write(head(mtcars), "mtcars", metadata = list("Hadley" = TRUE))
#'
#' # Get the pin
#' b %>% pin_read("mtcars")
#' # Get its metadata
#' b %>% pin_meta("mtcars")
#' # Get path to underlying data
#' b %>% pin_download("mtcars")
pin_meta <- function(board, name, version = NULL, hash = NULL, ...) {
  pin <- pin_retrieve(board, name, version = version, hash = hash, ...)
  structure(pin$meta, class = "pins_meta")
}

#' @export
print.pins_meta <- function(x, ...) {
  utils::str(unclass(x))
  invisible(x)
}

