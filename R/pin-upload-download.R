#' Upload and download files to and from a board
#'
#' This is a lower-level interface than `pin_read()` and `pin_write()` that
#' you can use to pin any file.
#'
#' @inheritParams pin_read
#' @return `pin_download()` returns a character vector of file paths;
#'   `pin_upload()` returns `board`, invisibly.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_upload(system.file("CITATION"))
#' path <- board %>% pin_download("CITATION")
#' path
#' readLines(path)[1:5]
pin_download <- function(board, name, version = NULL, hash = NULL, ...) {
  pin <- pin_retrieve(board, name, version = version, hash = hash, ...)
  pin$path
}

#' @export
#' @rdname pin_download
#' @param path A character vector of file paths to upload to `board`.
pin_upload <- function(board, path, name = NULL, desc = NULL, metadata = list(), ...) {
  check_board(board)
  if (!is.character(path)) {
    abort("`path` must be a character vector")
  }
  if (!all(fs::file_exists(path))) {
    abort("All elements of `path` must exist")
  }
  if (is.null(name) && length(path) == 1) {
    name <- fs::path_file(path)
    inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }

  metadata <- utils::modifyList(metadata, standard_meta(path, NULL, desc))
  board_pin_upload(board, name, path, metadata, ...)

  invisible(board)
}

