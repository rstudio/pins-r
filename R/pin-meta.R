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
pin_meta <- function(board, name, version = NULL, ...) {
  UseMethod("pin_meta")
}


new_meta <- function(x) {
  structure(x, class = "pins_meta")
}

#' @export
print.pins_meta <- function(x, ...) {
  utils::str(unclass(x))
  invisible(x)
}

