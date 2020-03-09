pin_download <- function(path,
                         name,
                         component,
                         extract = FALSE,
                         custom_etag = "",
                         remove_query = FALSE,
                         config = NULL,
                         headers = NULL,
                         can_fail = FALSE,
                         cache = TRUE,
                         content_length = 0,
                         ...) {
  must_download <- !cache

  # clean up name in case it's a full url
  name <- gsub("^https?://", "", name)

  local_path <- pin_storage_path(component, name)

  # use a temp path to rollback if something fails
  temp_path <- tempfile()
  dir.create(temp_path)
  on.exit(unlink(temp_path, recursive = TRUE))

  old_pin <- pin_registry_retrieve_maybe(name, component)
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
  catch_log <- function(e) tryCatch(e, error = function(e) { pin_log(e$message) ; NULL })
  catch_error <- if (old_cache_missing) function(e) e else function(e) tryCatch(e, error = function(e) { report_error(e$message) ; NULL })
  if (can_fail) report_error <- function(e) NULL

  cache <- list()
  cache$etag <- old_cache$etag
  cache$max_age <- if (!is.numeric(old_cache$max_age)) 0 else old_cache$max_age
  cache$change_age <- if (is.null(old_cache$change_age)) as.numeric(Sys.time()) - cache$max_age else old_cache$change_age
  cache$url <- path

  error <- NULL
  extract_type <- NULL

  pin_log("Checking 'change_age' header (time, change age, max age): ", as.numeric(Sys.time()), ", ", cache$change_age, ", ", cache$max_age)

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

    # skip downloading if etag has not changed
    if (old_cache_missing || !identical(old_cache$etag, cache$etag) || must_download) {
        download_name <- basename(path)

        if (remove_query) download_name <- strsplit(download_name, "\\?")[[1]][1]
        destination_path <- file.path(temp_path, download_name)
        pin_log("Downloading ", path, " to ", destination_path)

        write_spec <- httr::write_disk(destination_path, overwrite = TRUE)
        result <- catch_error(httr::GET(path, write_spec, headers, config, http_utils_progress(size = content_length)))
        extract_type <- gsub("application/(x-)?", "", result$headers$`content-type`)
        if (!is.null(result$headers$`content-type`) && result$headers$`content-type` %in% c("application/octet-stream", "application/zip")) {
          if (file.size(destination_path) > 4 &&
              identical(readBin(destination_path, raw(), 4), as.raw(c(0x50, 0x4b, 0x03, 0x04))))
            extract_type <- "zip"
        }

        if (httr::http_error(result)) {
          error <- paste0(httr::http_status(result)$message, ". Failed to download remote file: ", path)
          pin_log(as.character(httr::content(result)))

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

  for (file in dir(temp_path, full.names = TRUE)) {
    file.copy(file, local_path, overwrite = TRUE, recursive = TRUE)
  }

  # use relative paths to match remote service downloads and allow moving pins foldeer, potentially
  relative_path <- gsub(pin_storage_path(component, ""), "", local_path, fixed = TRUE)

  pin_registry_update(
    name = name,
    params = list(
      path = if (is.null(old_pin$path)) relative_path else old_pin$path,
      cache = new_cache),
    component = component)

  local_path
}
