#' Retrieve metadata for a pin
#'
#' @description
#' Pin metadata comes from three sources:
#'
#' * Standard metadata added by `pin_upload()`/`pin_write()`.
#'
#' * Metadata supplied by the user, stored in `$user`.
#'
#' * Local metadata generated when caching the pin, stored in `$local`.
#'   This includes information like the version of the pin, and the path
#'   its local cache.
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
  check_board(board)
  UseMethod("pin_meta")
}


# All pin_meta() methods should use `local_meta()` to ensure that results
# are stored in a consistent way
local_meta <- function(x, dir, version, ...) {
  x$local <- list(
    dir = dir,
    version = version,
    ...
  )
  structure(x, class = "pins_meta")
}

#' @export
print.pins_meta <- function(x, ...) {
  utils::str(unclass(x))
  invisible(x)
}
