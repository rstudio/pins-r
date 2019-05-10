file_remote <- function() {
  tryCatch({

  }, error = function(e) {
    FALSE
  })
}
pin_pack.character <- function(x, name, board, ...) {
  local_path <- paste0(
    file.path(pins:::pins_local_path("files"), name),
    ".",
    tools::file_ext(x))

  report_error <- if (file.exists(local_path)) warning else stop

  if (grepl("^http", x)) {
    status <- tryCatch(httr::status_code(httr::HEAD(x, httr::timeout(5))), error = function(e) e$message)
    error <- NULL

    if (is.character(status)) error <- paste0(status, ": ", x)
    if (status != 200) error <- paste0(status, " Failed to download remote file: ", x)

    if (!is.null(error)) {
      report_error(error)
    }
    else {
      httr::GET(x, httr::write_disk(local_path, overwrite = TRUE))
    }
  } else if (file.exists(x)) {
    file.copy(x, local_path)
  }
  else {
    report_error("Could not retrieve file from: ", x)
  }

  attr(local_path, "pin_type") <- "files"
  local_path
}

pin_unpack.files_pin <- function(x, board, name, ...) {
  x
}
