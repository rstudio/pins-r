board_initialize.local <- function(board, ...) {
  board
}

board_pin_create.local <- function(board, path, name, description, type, metadata, file, ...) {
  on.exit(board_connect(board$name))
  extension <- tools::file_ext(path)

  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(path)))
  must_cache <- identical(list(...)$cache, FALSE)

  old_pin <- tryCatch(board_yaml_pin_retrieve(name, "local"), error = function(e) NULL)
  old_metadata <- if (is.null(attr(old_pin, "pin_metadata"))) list() else attr(old_pin, "pin_metadata")

  report_error <- if (is.null(old_pin)) stop else warning

  local_path <- NULL

  metadata <- jsonlite::fromJSON(metadata)
  metadata$extension <- extension
  metadata$etag <- old_metadata$etag
  metadata$max_age <- if (!is.numeric(old_metadata$max_age)) 0 else old_metadata$max_age
  metadata$change_age <- if (is.null(old_metadata$change_age)) as.numeric(Sys.time()) - metadata$max_age else old_metadata$change_age

  if (grepl("^http", path)) {
    error <- NULL

    # skip downloading if max-age still valid
    if (as.numeric(Sys.time()) >= metadata$change_age + metadata$max_age || must_cache) {
      head_result <- httr::HEAD(path, httr::timeout(5))
      metadata$etag <- head_result$headers$etag
      metadata$max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

      status <- tryCatch(httr::status_code(head_result), error = function(e) e$message)
      metadata$change_age <- as.numeric(Sys.time())

      # skip downloading if etag has not changed
      if (is.null(old_metadata) || is.null(old_metadata$etag) || !identical(old_metadata$etag, metadata$etag) || must_cache) {
        if (is.character(status)) error <- paste0(status, ": ", path)
        if (status != 200) error <- paste0(status, " Failed to download remote file: ", path)

        if (!is.null(error)) {
          report_error(error)
        }
        else {
          local_path <- tempfile(fileext = pin_file_extension(path))
          httr::GET(path, httr::write_disk(local_path, overwrite = TRUE))
          on.exit(unlink(local_path))
        }
      }
    }

    if (is.null(error)) {
      # update change_age since we checked no change in HEAD
      board_yaml_pin_update_metadata(name, "local", jsonlite::toJSON(metadata, auto_unbox = TRUE))
    }
  }
  else {
    local_path <- path
  }

  if (is.null(local_path) || !file.exists(local_path)) {
    if (!is.null(local_path)) report_error("File does not exist: ", local_path)
    return(board_pin_get(board, name))
  }

  board_yaml_pin_remove(name, component = "local", TRUE)
  final_path <- board_yaml_pin_create(
    name = name,
    description = description,
    type = type,
    metadata = jsonlite::toJSON(metadata, auto_unbox = TRUE),
    component = "local",
    extension = paste0(".", extension))
  file.copy(local_path, final_path)

  pin_get(name, board$name)
}

board_pin_find.local <- function(board, text, ...) {
  board_yaml_pin_find(text, "local")
}

board_pin_get.local <- function(board, name, details) {
  board_yaml_pin_retrieve(name, "local")
}

board_pin_remove.local <- function(board, name) {
  board_yaml_pin_remove(name, "local")
}
