#' Pin versions
#'
#' List versions available for a pin. See `vignette("versioning)"` for details.
#'
#' @param board,name A pair of board and pin name. You can supply in either
#'   order: supply `name` then optionally `board` for legacy boards, and
#'   `board` then `name` for modern boards.
#' @param full `r lifecycle::badge("deprecated")`
#' @param ... Additional arguments passed on to methods for a specific board.
#' @return A data frame with at least a `version` column. Some boards may
#'   provided additional data.
#' @examples
#' board <- board_temp(versions = TRUE)
#'
#' board %>% pin_write(data.frame(x = 1:5), name = "df")
#' board %>% pin_write(data.frame(x = 2:6), name = "df")
#' board %>% pin_write(data.frame(x = 3:7), name = "df")
#'
#' # pin_read() returns the latest version by default
#' board %>% pin_read("df")
#'
#' # but you can return earlier versions if needed
#' board %>% pin_versions("df")
#'
#' ver <- pin_versions(board, "df")$version[[1]]
#' board %>% pin_read("df", version = ver)
#' @export
pin_versions <- function(board, name, ..., full = deprecated()) {
  ellipsis::check_dots_used()
  if (lifecycle::is_present(full)) {
    lifecycle::deprecate_warn("1.0.0", "pin_versions(full)")
  }

  if (missing(name) || is.board(name) || name %in% board_list()) {
    swap <- board
    board <- if (missing(name)) NULL else name
    board <- board_get(board)
    name <- swap

    if (!0 %in% board$api) {
      abort("Please supply `board` then `name` when working with modern boards")
    }

    board_pin_versions(board, name)
  } else {
    if (!1 %in% board$api) {
      abort("Please supply `name` then `board` when working with legacy boards")
    }

    UseMethod("pin_versions")
  }
}

#' @export
pin_versions.pins_board <- function(board, name, ...) {
  abort("board_url() doesn't support versions")
}
