#' Retrieve metadata for a pin
#'
#' @inheritParams pin_read
#' @returns A list.
#' @export
#' @examples
#' b <- board_temp()
#' b %>% pin_write(head(mtcars), "mtcars", metadata = list("Hadley" = TRUE))
#'
#' # Get the pin
#' b %>% pin_read("mtcars")
#' # Get its metadata
#' b %>% pin_meta("mtcars")
#' # Get path to underlying data
#' b %>% pin_download("mtcars")
pin_meta <- function(board, name, version = NULL, ...) {
  check_board(board)
  UseMethod("pin_meta")
}


new_meta <- function(x) {
  structure(x, class = "pins_meta")
}

#' @export
print.pins_meta <- function(x, ...) {
  utils::str(unclass(x))
  invisible(x)
}


multi_meta <- function(board, names) {
  meta <- map(names, pin_meta, board = board)

  wibble(
    name = names,
    version = as.integer(map_dbl(meta, ~ .x$api_version %||% NA_real_)),
    type = map_chr(meta, ~ .x$type %||% NA_character_),
    description = map_chr(meta, ~ .x$description %||% ""),
    created = rsc_parse_time(map_chr(meta, ~ .x$date %||% NA_character_)),
    file_size = fs::as_fs_bytes(map_int(meta, ~ sum(.x$file_size))),
    meta = meta
  )
}
