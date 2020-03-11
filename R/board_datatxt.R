datatxt_refresh_index <- function(board) {
  if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")

  index_url <- file.path(board$url, "data.txt")

  temp_index <- tempfile()
  response <- httr::GET(index_url,
                        httr::write_disk(temp_index, overwrite = TRUE),
                        board_datatxt_headers(board, "data.txt"))

  local_index <- file.path(board_local_storage(board$name, board = board), "data.txt")
  current_index <- board_manifest_get(local_index, default_empty = TRUE)

  if (httr::http_error(response)) {
    if (!identical(board$needs_index, FALSE)) {
      stop("Failed to retrieve data.txt file from ", board$url)
    }
  }
  else {
    new_index <- board_manifest_get(temp_index)
    current_names <- sapply(current_index, function(e) e$name)
    for (new_entry in new_index) {
      if (!new_entry$name %in% current_names) {
        current_index[[length(current_index) + 1]] <- new_entry
      }
    }
  }

  yaml::write_yaml(current_index, local_index)
}

board_initialize.datatxt <- function(board,
                                     headers = NULL,
                                     cache = board_cache_path(),
                                     url = NULL,
                                     needs_index = TRUE,
                                     browse_url = url,
                                     ...) {
  if (identical(url, NULL)) stop("The 'datatxt' board requires a 'url' parameter.")

  board$url <- gsub("/?data\\.txt$|/$", "", url)
  board$headers <- headers
  board$needs_index <- needs_index
  board$borwse_url <- browse_url

  for (key in names(list(...))) {
    board[[key]] <- list(...)[[key]]
  }

  datatxt_refresh_index(board)

  board
}

board_pin_get.datatxt <- function(board, name, extract = NULL, version = NULL, ...) {
  index <- board_manifest_get(file.path(board_local_storage(board$name), "data.txt"))
  index <- Filter(function(e) identical(e$name, name), index)

  local_path <- pin_storage_path(board$name, name)

  if (length(index) == 0 && identical(board$needs_index, TRUE)) {
    stop("Could not find '", name, "' pin in '", board$name, "' board.")
  }

  index_entry <- NULL
  if (length(index) > 0) {
    index_entry <- index[[1]]
  }
  else {
    # if there is no index, fallback to downloading data.txt for the pin,
    # this can happen with incomplete indexes.
    index_entry <- list(path = name)
  }

  # try to download index as well
  path_guess <- if (grepl(".*/.*\\.[a-zA-Z]+$", index_entry$path[1])) dirname(index_entry$path[1]) else index_entry$path[1]

  # if `path_guess` already has a scheme, don't prepend board URL
  path_guess <- if (grepl("^https?://", path_guess)) {
    path_guess
  } else {
    file.path(board$url, path_guess, fsep = "/")
  }
  download_path <- file.path(path_guess, "data.txt")

  pin_download(download_path, name, board$name, can_fail = TRUE, headers = board_datatxt_headers(board, download_path))
  manifest <- pin_manifest_get(local_path)

  if (!is.null(version)) {
    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    download_path <- file.path(path_guess, version, "data.txt")
    local_path <- file.path(local_path, version)
    pin_download(download_path, local_path, board$name, can_fail = TRUE, headers = board_datatxt_headers(board, download_path))
    manifest <- pin_manifest_get(local_path)
    path_guess <- file.path(path_guess, version)
  }

  if (!is.null(manifest)) {
    download_paths <- index_entry$path

    manifest_paths <- pin_manifest_download(local_path)
    if (!is.null(manifest_paths)) {
      # we find a data.txt file in subfolder with paths, we use those paths instead of the index paths.
      download_paths <- c()
      for (path in manifest_paths) {
        if (grepl("^https?://", path))
          download_paths <- c(download_paths, path)
        else
          download_paths <- c(download_paths, file.path(path_guess, path))
      }
    }
    else {
      index_entry$path <- NULL
      pin_manifest_create(local_path, index_entry, index_entry$path)
    }
  }
  else {
    # attempt to download from path when index not available
    download_paths <- file.path(board$url, name)
  }

  for (path in download_paths) {
    if (!grepl("https?://", path)) {
      path <- file.path(board$url, path)
    }

    local_path <- pin_download(path,
                               name,
                               board$name,
                               extract = identical(extract, TRUE),
                               headers = board_datatxt_headers(board, path))
  }

  local_path
}

board_pin_find.datatxt <- function(board, text, name, extended = FALSE, ...) {
  datatxt_refresh_index(board)

  entries <- board_manifest_get(file.path(board_local_storage(board$name), "data.txt"))

  if (identical(extended, TRUE)) return(pin_entries_to_dataframe(entries))

  results <- data.frame(
    name = sapply(entries, function(e) if (is.null(e$name)) basename(e$path) else e$name),
    description = sapply(entries, function(e) if (is.null(e$description)) "" else e$description),
    type = sapply(entries, function(e) if (is.null(e$type)) "files" else e$type),
    metadata = sapply(entries, function(e) jsonlite::toJSON(e, auto_unbox = TRUE)),
    stringsAsFactors = FALSE)

  if (is.character(name)) {
    results <- results[results$name == name,]
  }

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    path_guess <- if (grepl("\\.[a-zA-Z]+$", metadata$path)) dirname(metadata$path) else metadata$path
    datatxt_path <- file.path(board$url, path_guess[[1]], "data.txt")

    response <- httr::GET(datatxt_path, board_datatxt_headers(board, datatxt_path))
    if (!httr::http_error(response)) {
      pin_metadata <- board_manifest_load(datatxt_response_content(response))

      metadata <- pin_manifest_merge(metadata, pin_metadata)

      results$metadata <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
    }
  }

  results
}

datatxt_response_content <- function(response) {
  content <- httr::content(response)
  if (is.raw(content)) {
    content <- rawToChar(content)
  }

  content
}

datatxt_update_index <- function(board, path, operation, name = NULL, metadata = NULL) {
  index_url <- file.path(board$url, "data.txt")
  response <- httr::GET(index_url, board_datatxt_headers(board, index_url))

  index <- list()
  if (!httr::http_error(response)) {
    content <- datatxt_response_content(response)

    index <- board_manifest_load(content)
  }

  index_matches <- sapply(index, function(e) identical(e$path, path))
  index_pos <- if (length(index_matches) > 0) which(index_matches) else length(index) + 1
  if (length(index_pos) == 0) index_pos <- length(index) + 1

  if (identical(operation, "create")) {
    metadata$columns <- NULL

    index[[index_pos]] <- c(
      list(path = path),
      if (!is.null(name)) list(name = name) else NULL,
      metadata
    )
  }
  else if (identical(operation, "remove")) {
    if (index_pos <= length(index)) index[[index_pos]] <- NULL
  }
  else {
    stop("Operation ", operation, " is unsupported")
  }

  index_file <- file.path(board_local_storage(board$name, board = board), "data.txt")
  board_manifest_create(index, index_file)

  response <- httr::PUT(index_url,
                        body = httr::upload_file(normalizePath(index_file)),
                        board_datatxt_headers(board, "data.txt", verb = "PUT", file = normalizePath(index_file)))

  if (httr::http_error(response)) {
    stop("Failed to update data.txt file: ", datatxt_response_content(response))
  }
}

datatxt_upload_files <- function(board, name, files, path) {
  for (file in files) {
    subpath <- file.path(name, file)
    upload_url <- file.path(board$url, subpath)

    file_path <- normalizePath(file.path(path, file))
    response <- httr::PUT(upload_url,
                          body = httr::upload_file(file_path),
                          http_utils_progress("up", size = file.info(file_path)$size),
                          board_datatxt_headers(board, subpath, verb = "PUT", file = file_path))

    if (httr::http_error(response))
      stop("Failed to upload '", file, "' to '", upload_url, "'. Error: ", datatxt_response_content(response))
  }
}

board_pin_create.datatxt <- function(board, path, name, metadata, ...) {

  board_versions_create(board, name, path)

  upload_files <- dir(path, recursive = TRUE)

  datatxt_upload_files(board = board,
                       name = name,
                       files = upload_files,
                       path = path)

  datatxt_update_index(board = board,
                       path = name,
                       operation = "create",
                       name = name,
                       metadata = metadata)
}

board_pin_remove.datatxt <- function(board, name, ...) {
  files <- pin_files(name, board = board$name, absolute = FALSE)

  # also attempt to delete data.txt
  files <- c(files, file.path(name, "data.txt"))

  for (file in files) {
    delete_url <- file.path(board$url, file)

    response <- httr::DELETE(delete_url,
                             board_datatxt_headers(board, file, verb = "DELETE"))

    if (httr::http_error(response))
      warning("Failed to remove '", file, "' from '", board$name, "' board. Error: ", datatxt_response_content(response))
  }

  datatxt_update_index(board = board,
                       path = name,
                       operation = "remove",
                       name = name)
}

board_browse.datatxt <- function(board) {
  utils::browseURL(board$borwse_url)
}

board_pin_versions.datatxt <- function(board, name) {
  board_versions_get(board, name)
}
