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

  if (grepl("^http", x)) {
    status <- httr::status_code(httr::HEAD(x))
    if (status != 200) stop("Failed to download remote file with status code")
    httr::GET(x, httr::write_disk(local_path, overwrite = TRUE))
  } else if (file.exists(x)) {
    file.copy(x, local_path)
  }
  else {
    stop("Could not retrieve file from: ", x)
  }

  attr(local_path, "pin_type") <- "files"
  local_path
}

pin_unpack.files_pin <- function(x, board, name, ...) {
  x
}
