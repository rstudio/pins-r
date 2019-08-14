pin_load.files <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)

  files[!grepl("data\\.txt$", files)]
}

#' @keywords internal
#' @export
pin_preview.files <- function(x, board = NULL, ...) {
  data.frame(
    files = x,
    stringsAsFactors = FALSE
  )
}
