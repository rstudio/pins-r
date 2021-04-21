#' Search for pins
#'
#'
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
  UseMethod("pin_search")
}

#' @export
pin_search.pins_board <- function(board, pattern = NULL) {
  names <- pin_list(board)
  out <- multi_meta(board, names)

  if (!is.null(pattern)) {
    match <- grepl(pattern, out$name, perl = TRUE) |
      grepl(pattern, out$description, perl = TRUE)
    out <- out[match, , drop = FALSE]
  }

  out
}

