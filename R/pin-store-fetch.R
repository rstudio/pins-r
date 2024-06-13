#' Fetch/store a pin
#'
#' These are low-level functions that power [pin_read()], [pin_write()],
#' [pin_upload()], and [pin_download()]. They are needed primarily for folks
#' developing new board types, and should not generally be called directly.
#'
#' @return `pin_fetch()` is called primarily for its side-effect of downloading
#'   remote pins into the local cache. It returns the same data as [pin_meta].
#'   `pin_store()` is called for its side-effect of uploading a local file
#'   to a remote board. It invisibly returns the fully qualified pin name.
#'
#' @export
#' @keywords internal
#' @inheritParams pin_read
pin_fetch <- function(board, name, version = NULL, ...) {
  check_dots_used()
  UseMethod("pin_fetch")
}

#' @export
#' @rdname pin_fetch
#' @inherit pin_upload
pin_store <- function(board, name, paths, metadata, versioned = NULL, x = NULL, ...) {
  check_dots_used()
  UseMethod("pin_store")
}
