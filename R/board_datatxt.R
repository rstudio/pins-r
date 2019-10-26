board_url_update_index <- function(board, allow_empty = FALSE) {
  local_index <- file.path(board_local_storage(board$name, board = board), "data.txt")

  if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")

  index_url <- file.path(board$url, "data.txt")
  response <- httr::GET(index_url,
                        httr::write_disk(local_index, overwrite = TRUE),
                        headers = board_headers(board, index_url))

  if (httr::http_error(response) && !identical(allow_empty, TRUE)) stop("Failed to retrieve data.txt file from ", board$url)
}

board_initialize.datatxt <- function(board, headers = NULL, allow_empty = FALSE, ...) {
  board$url <- list(...)$url
  if (identical(board$url, NULL)) stop("The 'datatxt' board requires a 'url' parameter.")
  board$url <- gsub("/?data\\.txt$", "", board$url)
  board$headers <- headers

  board_url_update_index(board, allow_empty = allow_empty)

  board
}

board_pin_get.datatxt <- function(board, name, ...) {
  index <- board_manifest_get(file.path(board_local_storage(board$name), "data.txt"))
  index <- Filter(function(e) identical(e$name, name), index)

  local_path <- pin_storage_path(board$name, name)

  if (length(index) == 0) stop("Could not find '", name, "' pin in '", board$name, "' board.")

  download_paths <- index[[1]]$path

  # try to download index as well
  path_guess <- if (grepl("\\.[a-zA-Z]+$", index[[1]]$path[1])) dirname(index[[1]]$path[1]) else index[[1]]$path[1]
  download_path <- file.path(board$url, path_guess, "data.txt")
  pin_download(download_path, name, board$name, can_fail = TRUE, headers = board_headers(board, download_path))

  manifest <- pin_manifest_get(local_path)
  if (!is.null(manifest$path)) {
    # we find a data.txt file in subfolder with paths, we use those paths instead of the index paths.
    download_paths <- c()
    for (path in manifest$path) {
      if (grepl("^https?://", path))
        download_paths <- c(download_paths, path)
      else
        download_paths <- c(download_paths, file.path(board$url, path_guess, path))
    }
  }
  else {
    index[[1]]$path <- NULL
    pin_manifest_create(local_path, index[[1]], index[[1]]$path)
  }

  for (path in download_paths) {
    if (!grepl("https?://", path)) {
      path <- file.path(board$url, path)
    }

    local_path <- pin_download(path, name, board$name, headers = board_headers(board, path))
  }

  local_path
}

board_pin_find.datatxt <- function(board, text, ...) {
  board_url_update_index(board)

  entries <- board_manifest_get(file.path(board_local_storage(board$name), "data.txt"))

  results <- data.frame(
    name = sapply(entries, function(e) if (is.null(e$name)) basename(e$path) else e$name),
    description = sapply(entries, function(e) if (is.null(e$description)) "" else e$description),
    type = sapply(entries, function(e) if (is.null(e$type)) "files" else e$type),
    metadata = sapply(entries, function(e) jsonlite::toJSON(e, auto_unbox = TRUE)),
    stringsAsFactors = FALSE)

  if (is.character(text)) {
    results <- results[grepl(text, results$name),]
  }

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    path_guess <- if (grepl("\\.[a-zA-Z]+$", metadata$path)) dirname(metadata$path) else metadata$path
    response <- httr::GET(file.path(board$url, path_guess[[1]], "data.txt"))
    if (!httr::http_error(response)) {
      metadata <- c(metadata, board_manifest_load(httr::content(response)))
      results$metadata <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
    }
  }

  results
}

board_pin_create.datatxt <- function(board, path, name, metadata, ...) {
  stop("The 'url' board does not support creating pins.")
}

board_pin_remove.datatxt <- function(board, name, ...) {
  stop("The 'url' board does not support removing pins.")
}
