pin_download_files <- function(path, ...) {
  for (p in path) {
    if (length(path) > 1) pin_log("Downloading ", p, " from ", length(path), " downloads.")
    local_path <- pin_download_one(p, ...)
  }

  local_path
}

pin_download_one <- function(path,
                             name,
                             board,
                             extract = FALSE,
                             custom_etag = "",
                             remove_query = FALSE,
                             config = NULL,
                             headers = NULL,
                             can_fail = FALSE,
                             cache = TRUE,
                             content_length = 0,
                             subpath = name,
                             details = new.env(),
                             download = TRUE,
                             download_name = NULL,
                             ...) {
  stopifnot(is.board(board))
  must_download <- !cache

  custom_etag <- if (is.na(custom_etag)) "" else custom_etag

  # clean up name in case it's a full url
  name <- gsub("^https?://", "", name)

  local_path <- pin_registry_path(board, subpath)
  if (identical(download, FALSE)) {
    return(local_path)
  }

  # use a temp path to rollback if something fails
  temp_path <- tempfile()
  dir.create(temp_path)
  on.exit(unlink(temp_path, recursive = TRUE))

  old_pin <- tryCatch(pin_registry_retrieve(board, name), error = function(e) NULL)

  old_cache <- old_pin$cache
  old_cache_missing <- TRUE

  if (is.null(old_cache)) {
    old_pin$cache <- old_cache <- list()
    cache_index <- 1
  }
  else {
    cache_urls <- sapply(old_cache, function(e) e$url)
    cache_index <- which(cache_urls == path)
    if (length(cache_index) == 0) {
      old_cache <- list()
      cache_index <- length(cache_urls) + 1
    }
    else {
      old_cache <- old_cache[[cache_index]]
      old_cache_missing <- FALSE
    }
  }

  report_error <- if (old_cache_missing) stop else warning
  catch_log <- function(e) {
    tryCatch(e, error = function(e) {
      pin_log(e$message)
      NULL
    })
  }
  catch_error <- if (old_cache_missing) {
    function(e) e
  } else {
    function(e) {
      tryCatch(e, error = function(e) {
        report_error(e$message)
        NULL
      })
    }
  }
  if (can_fail) {
    report_error <- function(e) {
      details$error <- e
      NULL
    }
  }

  cache <- list()
  cache$etag <- old_cache$etag
  cache$max_age <- if (!is.numeric(old_cache$max_age)) 0 else old_cache$max_age
  cache$change_age <- if (is.null(old_cache$change_age)) as.numeric(Sys.time()) - cache$max_age else old_cache$change_age
  cache$url <- path

  error <- NULL
  extract_type <- NULL

  pin_log("Checking 'change_age' header (time, change age, max age): ", as.numeric(Sys.time()), ", ", cache$change_age, ", ", cache$max_age)
  details$something_changed <- FALSE

  # skip downloading if max-age still valid
  if (as.numeric(Sys.time()) >= cache$change_age + cache$max_age || must_download) {
    skip_download <- FALSE
    if (is.character(custom_etag) && nchar(custom_etag) > 0) {
      pin_log("Using custom 'etag' (old, new): ", old_cache$etag, ", ", custom_etag)
      cache$etag <- custom_etag
    }
    else {
      head_result <- catch_log(httr::HEAD(path, httr::timeout(5), headers, config))
      if (!is.null(head_result)) {
        cache$etag <- head_result$headers$etag
        cache$max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

        cache$change_age <- as.numeric(Sys.time())

        content_length <- head_result$headers$`content-length`

        pin_log("Checking 'etag' (old, new): ", old_cache$etag, ", ", cache$etag)
      }
    }

    etag_changed <- is.null(cache$etag) || !identical(old_cache$etag, cache$etag)

    # skip downloading if etag has not changed
    if (old_cache_missing || etag_changed || must_download) {
      if (identical(download_name, NULL)) download_name <- basename(path)

      if (remove_query) download_name <- strsplit(download_name, "\\?")[[1]][1]
      destination_path <- file.path(temp_path, download_name)
      pin_log("Downloading ", path, " to ", destination_path)
      details$something_changed <- TRUE

      write_spec <- httr::write_disk(destination_path, overwrite = TRUE)
      result <- catch_error(httr::GET(path, write_spec, headers, config, http_utils_progress(size = content_length)))
      extract_type <- gsub("application/(x-)?", "", result$headers$`content-type`)
      if (!is.null(result$headers$`content-type`) && result$headers$`content-type` %in% c("application/octet-stream", "application/zip")) {
        if (file.size(destination_path) > 4 &&
          identical(readBin(destination_path, raw(), 4), as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
          extract_type <- "zip"
        }
      }

      if (httr::http_error(result)) {
        error <- paste0(httr::http_status(result)$message, ". Failed to download remote file: ", path)
        pin_log(as.character(httr::content(result, encoding = "UTF-8")))

        report_error(error)
      }
    }
  }

  if (!is.null(error)) {
    return()
  }

  new_cache <- old_pin$cache
  new_cache[[cache_index]] <- cache

  # allow to override extraction method, useful in pin() from URLs.
  if (is.character(extract)) {
    extract_type <- extract
    extract <- TRUE
  }

  if (!is.null(extract_type) && identical(extract, TRUE)) {
    pin_extract(
      structure(dir(temp_path, full.names = TRUE), class = extract_type),
      temp_path
    )
  }

  fs::dir_create(local_path)
  for (file in dir(temp_path, full.names = TRUE)) {
    file.copy(file, local_path, overwrite = TRUE, recursive = TRUE)
  }

  # use relative paths to match remote service downloads and allow moving pins folder, potentially
  relative_path <- fs::path_rel(local_path, pin_registry_path(board))

  metadata <- list(
    path = if (is.null(old_pin$path)) relative_path else old_pin$path,
    cache = new_cache
  )
  pin_registry_update(board, name, metadata)

  local_path
}

# TODO: use simpler implementation.
# TODO: reconsider function choices
pin_extract <- function(file, destination) {
  UseMethod("pin_extract")
}

pin_extract.zip <- function(file, destination) {
  check_installed("zip")
  pin_log("Extracting zip file '", file, "'")
  zip::unzip(file, exdir = destination)
  unlink(file)
}

pin_extract.gzip <- function(file, destination) {
  if (length(find.package("R.utils", quiet = TRUE)) == 0) {
    warning("To extract gzip pins install the 'R.utils' package")
  } else {
    R.utils::gunzip(file, destname = file.path(destination, gsub(".gz", "", basename(file), fixed = TRUE)))
  }
}

`pin_extract.compressed-tar` <- function(file, destination) {
  pin_log("Extracting tgz file '", file, "'")
  utils::untar(file, exdir = destination)
  unlink(file)
}

pin_extract.default <- function(file, destination) {
  ext_map <- list(
    "\\.tar\\.gz$" = `pin_extract.compressed-tar`,
    "\\.zip$" = pin_extract.zip,
    "\\.gz$" = pin_extract.gzip
  )

  matches <- sapply(names(ext_map), function(e) grepl(e, file))
  if (any(matches)) ext_map[[names(which(matches)[1])]](file, destination)
}

pin_file_cache_max_age <- function(cache_control) {
  if (is.null(cache_control)) {
    return(NULL)
  }
  max_age <- grep("max-age", cache_control)
  if (length(max_age) != 1) {
    return(NULL)
  }

  max_age <- gsub(".*max-age=", "", cache_control)
  as.numeric(gsub(",.*$", "", max_age))
}
