#' List all pins in a board
#'
#' List names of all pins in a board. This is a low-level function; use
#' [pin_search()] to get more data about each pin in a convenient form.
#'
#' @inheritParams pin_write
#' @param ... Other arguments passed on to methods
#' @return A character vector
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_write(1:5, "x")
#' board %>% pin_write(letters, "y")
#' board %>% pin_write(runif(20), "z")
#'
#' board %>% pin_list()
#' board %>% pin_list_meta()
pin_list <- function(board, ...) {
  ellipsis::check_dots_used()
  UseMethod("pin_list")
}


#' Search for pins
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
  names <- pin_list(board)
  meta <- map(names, pin_meta, board = board)

  out <- wibble(
    name = names,
    type = map_chr(meta, ~ .x$type %||% NA_character_),
    description = map_chr(meta, ~ .x$description %||% ""),
    created = rsc_parse_time(map_chr(meta, ~ .x$date %||% NA_character_)),
    file_size = fs::as_fs_bytes(map_int(meta, ~ sum(.x$file_size))),
    meta = meta
  )

  if (!is.null(pattern)) {
    match <- grepl(pattern, out$name, perl = TRUE) |
      grepl(pattern, out$description, perl = TRUE)
    out <- out[match, , drop = FALSE]
  }

  out
}
