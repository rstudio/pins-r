#' @export
#' @keywords internal
pin.character <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(x)))

  board_object <- board_get(board)

  old_pin <- tryCatch(board_pin_get(board_object, name), error = function(e) NULL)
  report_error <- if (is.null(old_pin)) stop else warning

  local_path <- NULL
  if (grepl("^http", x)) {
    local_path <- tempfile(fileext = paste0(".", tools::file_ext(x)))
    status <- tryCatch(httr::status_code(httr::HEAD(x, httr::timeout(5))), error = function(e) e$message)
    error <- NULL

    if (is.character(status)) error <- paste0(status, ": ", x)
    if (status != 200) error <- paste0(status, " Failed to download remote file: ", x)

    if (!is.null(error)) {
      report_error(error)
    }
    else {
      httr::GET(x, httr::write_disk(local_path, overwrite = TRUE))
      on.exit(unlink(local_path))
    }
  }
  else {
    local_path <- x
  }

  if (file.exists(local_path)) {
    board_create_pin(board_object, local_path, name, description, "files", "")
  }
  else {
    report_error("File does not exist: ", local_path)
  }

  board_pin_get(board_object, name)
}
