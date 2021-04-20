#' Pin versions
#'
#' List versions available for a pin. See `vignette("versioning)"` for details.
#'
#' @inheritParams pin_read
#' @param full `r lifecycle::badge("deprecated")`
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
#' board %>% pin_read("df", version = "91e5f7f417660e3e")
#' @export
pin_versions <- function(board, name, full = deprecated()) {
  if (missing(name) || is.board(name) || name %in% board_list()) {
    warn("`pin_versions()` now takes `board` as first argument")
    swap <- board
    board <- if (missing(name)) NULL else name
    name <- swap
  }
  if (lifecycle::is_present(full)) {
    lifecycle::deprecate_warn("1.0.0", "pin_versions(full)")
  }

  board <- board_get(board)
  board_pin_versions(board, name)
}
