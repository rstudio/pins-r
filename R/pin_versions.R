
#' Pin Versions
#'
#' Retrieve versions available for a given pin.
#'
#' @param name The exact name of the pin to match when searching.
#' @param board The board name used to find the pin.
#' @param full Should the full versioned paths be shown? Defaults to `FALSE`.
#' @param ... Additional parameters.
#'
#' @examples
#' # define temporary board with versioning enabled
#' board <- board_local(tempfile(), versions = TRUE)
#'
#' # cache the mtcars dataset
#' pin(mtcars, name = "mtcars", board = board)
#'
#' # cache variation of the mtcars dataset
#' pin(mtcars * 10, name = "mtcars", board = board)
#'
#' # print the mtcars versions
#' versions <- pin_versions("mtcars", board = board)
#' versions
#'
#' # retrieve the original version
#' pin_get("mtcars", version = versions$version[[1]], board = board)
#'
#' # retrieve the variation version
#' pin_get("mtcars", version = versions$version[[2]], board = board)
#' @export
pin_versions <- function(name, board = NULL, full = FALSE, ...) {
  versions <- board_pin_versions(board_get(board), name)

  if (!full) {
    versions$version <- board_versions_shorten(versions$version)
  }

  format_tibble(versions)
}
