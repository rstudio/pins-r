#' Upload and download files to and from a board
#'
#' This is a lower-level interface than `pin_read()` and `pin_write()` that
#' works with paths. You can use this to upload any
#'
#' @export
pin_download <- function(board, name, ...) {
  board_pin_download(board, name)$path
}

#' @export
#' @rdname pin_download
pin_upload <- function(board, name, path, desc = NULL, metadata = list(), ...) {
  metadata <- utils::modifyList(metadata, standard_meta(path, NULL, desc))
  board_pin_upload(board, name, path, metadata)
}

