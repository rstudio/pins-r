#' Remote "data.txt" board (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Use board that for a website that uses the [data.txt](https://datatxt.org)
#' specification. A `data.txt` file is a YAML that provides some basic metadata
#' about a directory of files.
#'
#' @inheritParams new_board
#' @param url Path to the `data.txt` file or directory containing it.
#' @param headers Optional list of headers to include or a function to generate them.
#' @param needs_index Does this board have an index file?
#' @param browse_url Not currently used
#' @param index_updated Callback function used to update index
#' @param index_randomize When retrieving `data.txt` at a parameter with random
#'   query string to defeat caching?
#' @param path Subdirectory within `url`
#' @param versions Should this board be registered with support for versions?
#' @examples
#'
#' # register website board using datatxt file
#' board_register_datatxt(
#'   url = "https://datatxt.org/data.txt",
#'   name = "txtexample",
#'   cache = tempfile()
#' )
#'
#' # find pins
#' pin_find(board = "txtexample")
#' @export
#' @keywords internal
legacy_datatxt <- function(
  url,
  headers = NULL,
  cache = NULL,
  needs_index = TRUE,
  browse_url = url,
  index_updated = NULL,
  index_randomize = FALSE,
  path = NULL,
  versions = FALSE,
  name = NULL,
  ...
) {
  # use only subdomain as friendly name which is also used as cache folder
  name <- name %||% gsub("https?://|\\..*", "", url)
  cache <- cache %||% board_cache_path(name)

  board <- new_board_v0(
    "pins_board_datatxt",
    name = name,
    cache = cache,
    versions = versions,
    url = gsub("/?data\\.txt$|/$", "", url),
    headers = headers,
    needs_index = needs_index,
    browse_url = browse_url,
    index_updated = index_updated,
    index_randomize = index_randomize,
    subpath = path,
    ...
  )
  datatxt_refresh_index(board)
  board
}


#' @rdname legacy_datatxt
#' @export
board_register_datatxt <- function(
  url,
  name = NULL,
  headers = NULL,
  cache = NULL,
  ...
) {
  lifecycle::deprecate_warn(
    "1.4.0",
    "board_register_datatxt()",
    details = 'Learn more at <https://pins.rstudio.com/articles/pins-update.html>'
  )

  board <- legacy_datatxt(
    name = name,
    url = url,
    headers = headers,
    cache = cache,
    ...
  )
  board_register2(board)
}


file_path_null <- function(...) {
  paths <- list(...)
  paths <- Filter(Negate(is.null), paths)
  paths[["fsep"]] <- "/"
  do.call("file.path", paths)
}

datatxt_refresh_index <- function(board) {
  if (is.null(board$url)) stop("Invalid 'url' in '", board$name, "' board.")

  index_file <- "data.txt"
  if (identical(board$index_randomize, TRUE)) {
    index_file <- paste0(index_file, "?rand=", stats::runif(1) * 10^8)
  }

  index_url <- file_path_null(board$url, board$subpath, index_file)

  temp_index <- tempfile()
  response <- httr::GET(
    index_url,
    httr::write_disk(temp_index, overwrite = TRUE),
    board_datatxt_headers(board, "data.txt")
  )

  local_index <- pin_registry_path(board, "data.txt")
  current_index <- board_manifest_get(local_index, default_empty = TRUE)

  if (httr::http_error(response)) {
    if (!identical(board$needs_index, FALSE)) {
      stop("Failed to retrieve data.txt file from ", board$url)
    }
  } else {
    new_index <- board_manifest_get(temp_index)

    # retain cache when refreshing board to avoid redownloads after board_register
    new_index <- lapply(new_index, function(new_entry) {
      current_entry <- Filter(
        function(e) identical(e$path, new_entry$path),
        current_index
      )
      if (length(current_entry) == 1) {
        new_entry$cache <- current_entry[[1]]$cache
      }
      new_entry
    })

    current_index <- new_index
  }

  fs::dir_create(fs::path_dir(local_index))
  write_yaml(current_index, local_index)
}


datatxt_pin_download_info <- function(board, name, ...) {
  index <- board_manifest_get(pin_registry_path(board, "data.txt"))
  index <- Filter(function(e) identical(e$name, name), index)

  if (length(index) == 0 && identical(board$needs_index, TRUE)) {
    stop("Could not find '", name, "' pin in '", board$name, "' board.")
  }

  index_entry <- NULL
  if (length(index) > 0) {
    index_entry <- index[[1]]
  } else {
    # if there is no index, fallback to downloading data.txt for the pin,
    # this can happen with incomplete indexes.
    index_entry <- list(path = name)
  }

  # try to download index as well
  path_guess <- if (grepl(".*/.*\\.[a-zA-Z]+$", index_entry$path[1]))
    dirname(index_entry$path[1]) else index_entry$path[1]

  # if `path_guess` already has a scheme, don't prepend board URL
  path_guess <- if (grepl("^https?://", path_guess)) {
    path_guess
  } else {
    file_path_null(board$url, board$subpath, path_guess, fsep = "/")
  }

  list(
    path_guess = path_guess,
    index_entry = index_entry
  )
}

datatxt_refresh_manifest <- function(board, name, download = TRUE, ...) {
  pin_info <- datatxt_pin_download_info(board, name, ...)
  path_guess <- pin_info$path_guess
  index_entry <- pin_info$index_entry

  download_path <- file.path(path_guess, "data.txt")
  pin_download_files(
    download_path,
    name,
    board,
    can_fail = TRUE,
    headers = board_datatxt_headers(board, download_path),
    download = download
  )

  list(
    path_guess = path_guess,
    index_entry = index_entry,
    download_path = download_path
  )
}

#' @export
board_pin_get.pins_board_datatxt <- function(
  board,
  name,
  extract = NULL,
  version = NULL,
  download = TRUE,
  ...
) {
  manifest_paths <- datatxt_refresh_manifest(
    board,
    name,
    download = download,
    ...
  )
  path_guess <- manifest_paths$path_guess
  index_entry <- manifest_paths$index_entry
  download_path <- manifest_paths$download_path

  local_path <- pin_registry_path(board, name)
  manifest <- pin_manifest_get(local_path)

  if (!is.null(version)) {
    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    download_path <- file.path(path_guess, version, "data.txt")
    local_path <- file.path(local_path, version)
    pin_download_files(
      download_path,
      name,
      board,
      can_fail = TRUE,
      headers = board_datatxt_headers(board, download_path),
      subpath = file.path(name, version)
    )
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
        if (grepl("^https?://", path)) {
          download_paths <- c(download_paths, path)
        } else {
          download_paths <- c(download_paths, file.path(path_guess, path))
        }
      }
    } else {
      index_entry$path <- NULL
      pin_manifest_create(local_path, index_entry, index_entry$path)
    }
  } else {
    # attempt to download from path when index not available
    download_paths <- file_path_null(board$url, board$subpath, name)
  }

  for (path in download_paths) {
    if (!grepl("https?://", path)) {
      path <- file_path_null(board$url, board$subpath, path)
    }

    local_path <- pin_download_files(
      path,
      name,
      board,
      extract = identical(extract, TRUE),
      headers = board_datatxt_headers(board, path),
      download = download
    )
  }

  local_path
}

#' @export
board_pin_find.pins_board_datatxt <- function(
  board,
  text,
  name,
  extended = FALSE,
  ...
) {
  datatxt_refresh_index(board)

  entries <- board_manifest_get(pin_registry_path(board, "data.txt"))

  if (identical(extended, TRUE)) {
    return(pin_entries_to_dataframe(entries))
  }

  results <- data.frame(
    name = sapply(
      entries,
      function(e) if (is.null(e$name)) basename(e$path) else e$name
    ),
    description = sapply(
      entries,
      function(e) if (is.null(e$description)) "" else e$description
    ),
    type = sapply(
      entries,
      function(e) if (is.null(e$type)) "files" else e$type
    ),
    metadata = sapply(
      entries,
      function(e) jsonlite::toJSON(e, auto_unbox = TRUE)
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(text)) {
    find_names <- grepl(text, results$name, ignore.case = TRUE)
    find_description <- grepl(text, results$description, ignore.case = TRUE)
    results <- results[find_names | find_description, , drop = FALSE]
  }

  if (is.character(name)) {
    results <- results[results$name == name, ]
  }

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    path_guess <- if (grepl("\\.[a-zA-Z]+$", metadata$path))
      dirname(metadata$path) else metadata$path
    datatxt_path <- file_path_null(
      board$url,
      board$subpath,
      path_guess[[1]],
      "data.txt"
    )

    response <- httr::GET(
      datatxt_path,
      board_datatxt_headers(board, datatxt_path)
    )
    if (!httr::http_error(response)) {
      pin_metadata <- board_manifest_load(datatxt_response_content(response))

      metadata <- pin_manifest_merge(metadata, pin_metadata)

      results$metadata <- jsonlite::toJSON(metadata, auto_unbox = TRUE)
    }
  }

  results
}

datatxt_response_content <- function(response) {
  content <- httr::content(response, encoding = "UTF-8")
  if (is.raw(content)) {
    content <- rawToChar(content)
  }

  content
}

datatxt_update_index <- function(
  board,
  path,
  operation,
  name = NULL,
  metadata = NULL
) {
  index_file <- "data.txt"
  index_url <- file_path_null(board$url, board$subpath, index_file)

  index_file_get <- file_path_null(board$subpath, "data.txt")
  if (identical(board$index_randomize, TRUE)) {
    # some boards cache bucket files by default which can be avoided by changing the url
    index_file_get <- paste0(index_file, "?rand=", stats::runif(1) * 10^8)
  }

  response <- httr::GET(
    file.path(board$url, index_file_get),
    board_datatxt_headers(board, index_file_get)
  )

  index <- list()
  if (httr::http_error(response)) {
    if (identical(operation, "remove")) {
      stop(
        "Failed to retrieve latest data.txt file, the pin was partially removed."
      )
    }
  } else {
    content <- datatxt_response_content(response)

    index <- board_manifest_load(content)
  }

  index_matches <- sapply(index, function(e) identical(e$path, path))
  index_pos <- if (length(index_matches) > 0) which(index_matches) else
    length(index) + 1
  if (length(index_pos) == 0) index_pos <- length(index) + 1

  if (identical(operation, "create")) {
    metadata$columns <- NULL

    index[[index_pos]] <- c(
      list(path = path),
      if (!is.null(name)) list(name = name) else NULL,
      metadata
    )
  } else if (identical(operation, "remove")) {
    if (index_pos <= length(index)) index[[index_pos]] <- NULL
  } else {
    stop("Operation ", operation, " is unsupported")
  }

  index_file <- pin_registry_path(board, "data.txt")
  index_file_put <- file_path_null(board$subpath, "data.txt")
  board_manifest_create(index, index_file)

  response <- httr::PUT(
    index_url,
    body = httr::upload_file(normalizePath(index_file)),
    board_datatxt_headers(
      board,
      index_file_put,
      verb = "PUT",
      file = normalizePath(index_file)
    )
  )

  if (httr::http_error(response)) {
    stop("Failed to update data.txt file: ", datatxt_response_content(response))
  }

  if (!identical(board$index_updated, NULL) && identical(operation, "create")) {
    board$index_updated(board)
  }
}

datatxt_upload_files <- function(board, name, files, path) {
  for (file in files) {
    subpath <- file_path_null(board$subpath, name, file)
    upload_url <- file.path(board$url, subpath)

    file_path <- normalizePath(file.path(path, file))
    response <- httr::PUT(
      upload_url,
      body = httr::upload_file(file_path),
      http_utils_progress("up", size = file.info(file_path)$size),
      board_datatxt_headers(board, subpath, verb = "PUT", file = file_path)
    )

    if (httr::http_error(response)) {
      stop(
        "Failed to upload '",
        file,
        "' to '",
        upload_url,
        "'. Error: ",
        datatxt_response_content(response)
      )
    }
  }
}

datatxt_invalid_name <- function(name) {
  # see https://stackoverflow.com/questions/7116450/what-are-valid-s3-key-names-that-can-be-accessed-via-the-s3-rest-api
  if (grepl("[&@:,$=+?; \\^`><{}\\[#%~|]", name)) {
    stop(
      "The 'name' should not contain the following characters: '&@:,$=+?; \\^`><{}[]#%~|'"
    )
  }
}

#' @export
board_pin_create.pins_board_datatxt <- function(
  board,
  path,
  name,
  metadata,
  ...
) {
  datatxt_invalid_name(name)
  board_versions_create(board, name, path)

  upload_files <- dir(path, recursive = TRUE)

  datatxt_upload_files(
    board = board,
    name = name,
    files = upload_files,
    path = path
  )

  datatxt_update_index(
    board = board,
    path = name,
    operation = "create",
    name = name,
    metadata = metadata
  )
}

# Retrieve data.txt files, including versioned files
datatxt_pin_files <- function(board, name) {
  entry <- pin_find(name = name, board = board, metadata = TRUE)

  if (nrow(entry) != 1) stop("Pin '", name, "' not found.")
  metadata <- jsonlite::fromJSON(as.list(entry)$metadata)

  files <- metadata$path

  for (version in metadata$versions) {
    path_guess <- datatxt_pin_download_info(board, name)$path_guess

    download_path <- file.path(path_guess, version, "data.txt")
    local_path <- pin_registry_path(board, name, version)
    pin_download_files(
      download_path,
      name,
      board,
      can_fail = TRUE,
      headers = board_datatxt_headers(board, download_path),
      subpath = file.path(name, version)
    )
    manifest <- pin_manifest_get(local_path)

    files <- c(
      files,
      file.path(name, version, manifest$path),
      file.path(name, version, "data.txt")
    )
  }

  files
}

#' @export
board_pin_remove.pins_board_datatxt <- function(board, name, ...) {
  files <- datatxt_pin_files(board, name)

  # also attempt to delete data.txt
  files <- c(files, file.path(name, "data.txt"))

  for (file in files) {
    delete_path <- file_path_null(board$subpath, file)
    delete_url <- file.path(board$url, delete_path)

    response <- httr::DELETE(
      delete_url,
      board_datatxt_headers(board, delete_path, verb = "DELETE")
    )

    if (httr::http_error(response)) {
      warning(
        "Failed to remove '",
        delete_path,
        "' from '",
        board$name,
        "' board. Error: ",
        datatxt_response_content(response)
      )
    }
  }

  datatxt_update_index(
    board = board,
    path = name,
    operation = "remove",
    name = name
  )

  unlink(pin_registry_path(board, name), recursive = TRUE)
}

#' @export
board_browse.pins_board_datatxt <- function(board, ...) {
  utils::browseURL(board$borwse_url)
}

#' @export
board_pin_versions.pins_board_datatxt <- function(board, name, ...) {
  datatxt_refresh_manifest(board, name, ...)
  board_versions_get(board, name)
}


# Testing -----------------------------------------------------------------

local_legacy_datatxt <- function(..., env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)

  board_register_datatxt(
    ...,
    url = "https://raw.githubusercontent.com/rstudio/pins-r/master/tests/testthat/datatxt/data.txt",
    cache = path
  )
}


# Helpers -----------------------------------------------------------------

board_datatxt_headers <- function(board, path, verb = "GET", file = NULL) {
  if (!is.null(board$url)) {
    # remove base url form path since S3 and others require relative paths when using custom domains
    path <- gsub(paste0("^", board$url, "/?"), "", path)
  }

  if (is.list(board$headers)) {
    httr::add_headers(.headers = unlist(board$headers))
  } else if (is.character(board$headers)) {
    httr::add_headers(.headers = board$headers)
  } else if ("request" %in% class(board$headers) || is.null(board$headers)) {
    board$headers
  } else if (is.function(board$headers)) {
    board$headers(board, verb, path, file)
  } else {
    stop(
      "Unsupported '",
      class(board$headers)[[1]],
      "' class for board headers."
    )
  }
}

board_manifest_get <- function(path, default_empty = FALSE) {
  if (!file.exists(path) && default_empty) {
    return(list())
  }
  suppressWarnings(yaml::read_yaml(path, eval.expr = FALSE))
}

board_manifest_load <- function(manifest) {
  suppressWarnings(yaml::yaml.load(manifest, eval.expr = FALSE))
}

board_manifest_create <- function(index, file) {
  write_yaml(index, file)
}

pin_entries_to_dataframe <- function(entries) {
  jsonlite::fromJSON(jsonlite::toJSON(
    entries,
    null = "null",
    auto_unbox = TRUE
  ))
}
