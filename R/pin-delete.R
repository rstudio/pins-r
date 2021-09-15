#' Delete a pin
#'
#' Delete a pin (or pins), removing it from the board
#'
#' @inheritParams pin_read
#' @param names The names of one or more pins to delete
#' @details
#' Some boards require additional arguments when deleting pins. In particular, deleting pins in OneDrive and SharePoint Online (`board_onedrive`, `board_sharepoint` and `board_ms365`) may require passing `by_item=TRUE` to work around document protection policies that prohibit deleting non-empty folders.
#' @export
#' @examples
#' board <- board_temp()
#' board %>% pin_write(1:5, "x")
#' board %>% pin_write(mtcars)
#' board %>% pin_write(runif(1e6), "y")
#' board %>% pin_list()
#'
#' board %>% pin_delete(c("x", "y"))
#' board %>% pin_list()
pin_delete <- function(board, names, ...) {
  ellipsis::check_dots_used(...)
  check_board(board, "pin_delete()", "pin_remove()")
  UseMethod("pin_delete")
}
