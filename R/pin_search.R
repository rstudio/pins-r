#' Search for pins
#'
#' The underlying search method depends on the `board`, but most will search
#' for text in the pin name and description.
#'
#' @returns A data frame that summarises the metadata for each pin.
#'   Key attributes (`name`, `type`, `description`, `created`, and `file_size`)
#'   are pulled out into columns; everything else can be found in the `meta`
#'   list-column.
#' @inheritParams pin_read
#' @param search A string to search for in pin name and description.
#' @param ... Additional arguments passed on to methods.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_write(1:5, "x", desc = "Some numbers")
#' board %>% pin_write(letters[c(1, 5, 10, 15, 21)], "y", desc = "My favourite letters")
#' board %>% pin_write(runif(20), "z", desc = "Random numbers")
#'
#' board %>% pin_search("number")
#' board %>% pin_search("letters")
pin_search <- function(board, search, ...) {
  check_board(board, "pin_search()", "pin_find()")
  ellipsis::check_dots_used()
  UseMethod("pin_search")
}

#' @export
pin_search.pins_board <- function(board, search, ...) {
  names <- pin_list(board)
  out <- multi_meta(board, names)

  match <- grepl(search, out$name, fixed = TRUE) |
    grepl(search, out$description, fixed = TRUE)
  out <- out[match, , drop = FALSE]

  out
}
