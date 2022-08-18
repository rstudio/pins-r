#' Upload and download files to and from a board
#'
#' This is a lower-level interface than `pin_read()` and `pin_write()` that
#' you can use to pin any file, as opposed to any R object. The path returned
#' by `pin_download()` is a read-only path to a cached file: you should never
#' attempt to modify this file.
#'
#' @inheritParams pin_read
#' @return `pin_download()` returns a character vector of file paths;
#'   `pin_upload()` returns the fully qualified name of the new pin, invisibly.
#' @export
#' @examples
#' board <- board_temp()
#'
#' board %>% pin_upload(system.file("CITATION"))
#' path <- board %>% pin_download("CITATION")
#' path
#' readLines(path)[1:5]
pin_download <- function(board, name, version = NULL, hash = NULL, ...) {
  check_board(board, "pin_download()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)
  check_hash(meta, hash)

  as.character(fs::path(meta$local$dir, meta$file))
}

#' @export
#' @rdname pin_download
#' @param paths A character vector of file paths to upload to `board`.
pin_upload <- function(board, paths, name = NULL, title = NULL, description = NULL, metadata = NULL, ...) {
  check_board(board, "pin_upload()", "pin()")

  if (!is.character(paths)) {
    abort("`path` must be a character vector")
  }
  if (!all(fs::file_exists(fs::path_expand(paths)))) {
    abort("All elements of `paths` must exist")
  }
  if (any(fs::path_file(paths) == "data.txt")) {
    abort("Can't pin file called `data.txt`")
  }

  if (is.null(name) && length(paths) == 1) {
    name <- fs::path_file(paths)
    inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }

  # Expand any directories
  is_dir <- fs::is_dir(paths)
  if (any(is_dir)) {
    paths <- as.list(paths)
    paths[is_dir] <- map(paths[is_dir], fs::dir_ls, recurse = TRUE, type = c("file", "symlink"))
    paths <- as.character(unlist(paths, use.names = FALSE))
  }

  meta <- standard_meta(
    paths = paths,
    type = "file",
    title = title %||% default_title(name, path = paths),
    description = description
  )
  meta$user <- metadata

  invisible(pin_store(board, name, paths, meta, ...))
}

