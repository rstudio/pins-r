#' Search for pins
#'
#' The underlying search method depends on the `board`, but most will search
#' for text in the pin name and title.
#'
#' @returns A data frame that summarises the metadata for each pin.
#'   Key attributes (`name`, `type`, `description`, `created`, and `file_size`)
#'   are pulled out into columns; everything else can be found in the `meta`
#'   list-column.
#' @inheritParams pin_read
#' @param search A string to search for in pin name and title.
#'   Use `NULL` to return all pins.
#' @param ... Additional arguments passed on to methods.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_write(1:5, "x", title = "Some numbers")
#' board %>% pin_write(letters[c(1, 5, 10, 15, 21)], "y", title = "My favourite letters")
#' board %>% pin_write(runif(20), "z", title = "Random numbers")
#'
#' board %>% pin_search()
#' board %>% pin_search("number")
#' board %>% pin_search("letters")
pin_search <- function(board, search = NULL, ...) {
  check_board(board, "pin_search", "pin_find")
  ellipsis::check_dots_used()
  UseMethod("pin_search")
}

#' @export
pin_search.pins_board <- function(board, search = NULL, ...) {
  names <- pin_list(board)
  out <- multi_meta(board, names)

  if (!is.null(search)) {
    match <- grepl(search, out$name, fixed = TRUE) |
      grepl(search, out$title, fixed = TRUE)
    out <- out[match, , drop = FALSE]
  }

  out
}
