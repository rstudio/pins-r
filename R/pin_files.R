pin_load.files <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)

  result <- files[!grepl("data\\.txt$", files)]

  format_tibble(result)
}

#' @keywords internal
#' @export
pin_preview.files <- function(x, board = NULL, ...) {
  data.frame(
    files = x,
    stringsAsFactors = FALSE
  )
}
