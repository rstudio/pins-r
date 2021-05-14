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
  check_board(board, "pin_download()", "pin_get()")

  meta <- pin_fetch(board, name, version = version, ...)
  check_hash(meta, hash)

  as.character(fs::path(meta$local$dir, meta$file))
}

#' @export
#' @rdname pin_download
#' @param path A character vector of file paths to upload to `board`.
pin_upload <- function(board, path, name = NULL, desc = NULL, metadata = NULL, ...) {
  check_board(board, "pin_upload()", "pin()")

  if (!is.character(path)) {
    abort("`path` must be a character vector")
  }
  if (!all(fs::file_exists(path))) {
    abort("All elements of `path` must exist")
  }
  if (any(fs::path_file(path) == "data.txt")) {
    abort("Can pin file called `data.txt`")
  }

  if (is.null(name) && length(path) == 1) {
    name <- fs::path_file(path)
    inform(paste0("Guessing `name = '", name, "'`"))
  } else {
    check_name(name)
  }

  # Expand any directories
  is_dir <- fs::is_dir(path)
  if (any(is_dir)) {
    path <- as.list(path)
    path[is_dir] <- map(path[is_dir], fs::dir_ls, recurse = TRUE, type = c("file", "symlink"))
    path <- as.character(unlist(path, use.names = FALSE))
  }

  meta <- standard_meta(path, desc = desc, type = "file")
  meta$user <- metadata

  pin_store(board, name, path, meta, ...)
}

