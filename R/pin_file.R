pin_file_extension <- function(x) {
  extension <- tools::file_ext(x)
  if (nchar(extension) == 0) extension <- "txt"
  paste0(".", extension)
}

pin_file_cache_max_age <- function(cache_control) {
  if (is.null(cache_control)) return(NULL)
  max_age <- grep("max-age", cache_control)
  if (length(max_age) != 1) return(NULL)

  max_age <- gsub(".*max-age=", "", cache_control)
  as.numeric(gsub(",.*$", "", max_age))
}

#' @keywords internal
#' @export
pin.character <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  extension <- if (length(x) > 1) "zip" else tools::file_ext(x)
  board_pin_store(board, x, name, description, "files", list(extension = extension), ...)
}
