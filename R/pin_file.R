pin_file_extension <- function(x) {
  extension <- tools::file_ext(x)
  if (nchar(extension) == 0) extension <- "txt"
  paste0(".", extension)
}

pin_file_cache_max_age <- function(cache_control) {
  if (is.null(cache_control)) return(NULL)
  max_age <- grep("max-age", cache_control)
  if (length(max_age) != 1) return(NULL)
  as.numeric(gsub("max-age=", "", max_age))
}

#' @keywords internal
#' @export
pin.character <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  cache <- !identical(list(...)$cache, FALSE)
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(x)))

  board_object <- board_get(board)

  old_pin <- tryCatch(board_pin_get(board_object, name), error = function(e) NULL)
  metadata <- attr(old_pin, "pin_metadata")

  report_error <- if (is.null(old_pin)) stop else warning
  max_age <- if (is.null(metadata$max_age)) 0 else metadata$max_age
  change_age <- if (is.null(metadata$change_age)) as.numeric(Sys.time()) - max_age else metadata$change_age

  local_path <- NULL
  etag <- NULL

  if (grepl("^http", x)) {
    # skip downloading if max-age still valid
    if (as.numeric(Sys.time()) < change_age + max_age || !cache) {
      head_result <- httr::HEAD(x, httr::timeout(5))
      etag <- head_result$headers$etag
      max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

      status <- tryCatch(httr::status_code(head_result), error = function(e) e$message)
      error <- NULL

      # skip downloading if etag has not changed
      if (is.null(metadata) || is.null(metadata$etag) || !identical(metadata$etag, etag) || !cache) {
        if (is.character(status)) error <- paste0(status, ": ", x)
        if (status != 200) error <- paste0(status, " Failed to download remote file: ", x)

        if (!is.null(error)) {
          report_error(error)
        }
        else {
          local_path <- tempfile(fileext = pin_file_extension(x))
          httr::GET(x, httr::write_disk(local_path, overwrite = TRUE))
          on.exit(unlink(local_path))
        }
      }
    }
  }
  else {
    local_path <- x
  }

  if (is.null(local_path) || !file.exists(local_path)) {
    if (!is.null(local_path)) report_error("File does not exist: ", local_path)
    return(board_pin_get(board_object, name))
  }

  metadata <- list(
    extension = tools::file_ext(local_path),
    etag = etag,
    max_age = max_age,
    change_age = as.numeric(Sys.time())
  )

  board_pin_store(board_object, local_path, name, description, "files", metadata)
}
