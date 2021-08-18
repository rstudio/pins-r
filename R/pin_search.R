#' Search for pins
#'
#' The underlying search method depends on the `board`, but most will search
#' for text in the pin name and description.
#'
#' @returns A data frame that summarises the metadata for each pin.
#'   Key columns (`version`, `type`, `description`, `created`, and `file_size`)
#'   are pulled out into columns; everything else can be found in the `meta`
#'   list-column.
#' @inheritParams pin_read
#' @param pattern A string to search for in pin name and description.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_write(1:5, "x", desc = "Some numbers")
#' board %>% pin_write(letters[c(1, 5, 10, 15, 21)], "y", desc = "My favourite letters")
#' board %>% pin_write(runif(20), "z", desc = "Random numbers")
#'
#' board %>% pin_search()
#' board %>% pin_search("number")
#' board %>% pin_search("letters")
pin_search <- function(board, pattern = NULL) {
  check_board(board, "pin_search()", "pin_find()")
  UseMethod("pin_search")
}

#' @export
pin_search.pins_board <- function(board, pattern = NULL) {
  names <- pin_list(board)
  out <- multi_meta(board, names)

  if (!is.null(pattern)) {
    match <- grepl(pattern, out$name, fixed = TRUE) |
      grepl(pattern, out$description, fixed = TRUE)
    out <- out[match, , drop = FALSE]
  }

  out
}
