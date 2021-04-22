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

multi_meta <- function(board, names) {
  meta <- map(names, pin_meta, board = board)

  tibble::tibble(
    name = names,
    version = as.integer(map_dbl(meta, ~ .x$api_version %||% NA_real_)),
    type = map_chr(meta, ~ .x$type %||% NA_character_),
    description = map_chr(meta, ~ .x$description %||% ""),
    created = rsc_parse_time(map_chr(meta, ~ .x$date %||% NA_character_)),
    file_size = fs::as_fs_bytes(map_int(meta, ~ sum(.x$file_size))),
    meta = meta
  )
}
