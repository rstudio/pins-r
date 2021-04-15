#' Upload and download files to and from a board
#'
#' This is a lower-level interface than `pin_read()` and `pin_write()` that
#' you can use to pin any file.
#'
#' @inheritParams pin_read
#' @param ... Additional arguments passed on to board pin methods.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_upload(system.file("CITATION"))
#' path <- board %>% pin_download("CITATION")
#' path
#' readLines(path)[1:5]
pin_download <- function(board, name, ...) {
  check_board(board)
  check_name(name)

  board_pin_download(board, name, ...)$path
}

#' @export
#' @rdname pin_download
#' @param path Path to file to upload to `board`.
pin_upload <- function(board, path, name = NULL, desc = NULL, metadata = list(), ...) {
  check_board(board)
  if (!is_string(path)) {
    abort("`path` must be a string")
  } else if (!fs::file_exists(path)) {
    abort("`path` must exist")
  }
  if (is.null(name)) {
    name <- fs::path_file(path)
    inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }

  metadata <- utils::modifyList(metadata, standard_meta(path, NULL, desc))
  board_pin_upload(board, name, path, metadata, ...)
}

