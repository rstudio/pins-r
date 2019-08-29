pin_download <- function(path, name, component, ...) {
  must_download <- identical(list(...)$download, TRUE)
  headers <- list(...)$headers
  config <- list(...)$config
  custom_etag <- list(...)$custom_etag
  remove_query <- identical(list(...)$remove_query, TRUE)
  can_fail <- identical(list(...)$can_fail, TRUE)

  local_path <- pin_storage_path(component, name)

  # use a temp path to rollback if something fails
  temp_path <- tempfile()
  dir.create(temp_path)
  on.exit(unlink(temp_path, recursive = TRUE))

  old_pin <- tryCatch(pin_registry_retrieve(name, component), error = function(e) NULL)
  old_cache <- old_pin$cache
  old_cache_missing <- is.null(old_cache)

  if (old_cache_missing) {
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
    }
  }

  report_error <- if (old_cache_missing) stop else warning
  catch_error <- if (old_cache_missing) function(e) e else function(e) tryCatch(e, error = function(e) { report_error(e$message) ; NULL })
  if (can_fail) report_error <- function(e) NULL

  cache <- list()
  cache$etag <- old_cache$etag
  cache$max_age <- if (!is.numeric(old_cache$max_age)) 0 else old_cache$max_age
  cache$change_age <- if (is.null(old_cache$change_age)) as.numeric(Sys.time()) - cache$max_age else old_cache$change_age
  cache$url <- path

  error <- NULL
  is_zip <- FALSE

  pin_log("Checking 'change_age' header (time, change age, max age): ", as.numeric(Sys.time()), ", ", cache$change_age, ", ", cache$max_age)

  # skip downloading if max-age still valid
  if (as.numeric(Sys.time()) >= cache$change_age + cache$max_age || must_download) {

    status <- 200
    skip_download <- FALSE
    if (!is.null(custom_etag)) {
      pin_log("Using custom 'etag' (old, new): ", old_cache$etag, ", ", custom_etag)
      cache$etag <- custom_etag
    }
    else {
      head_result <- catch_error(httr::HEAD(path, httr::timeout(5), headers, config))
      if (is.null(head_result)) {
        skip_download <- TRUE
      }
      else {
        cache$etag <- head_result$headers$etag
        cache$max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

        status <- tryCatch(httr::status_code(head_result), error = function(e) e$message)
        cache$change_age <- as.numeric(Sys.time())

        pin_log("Checking 'etag' (old, new): ", old_cache$etag, ", ", cache$etag)
      }
    }

    # skip downloading if etag has not changed
    if (!skip_download && (old_cache_missing || !identical(old_cache$etag, cache$etag) || must_download)) {
      if (is.character(status)) error <- paste0(status, ": ", path)
      if (status != 200) error <- paste0(status, " Failed to download remote file: ", path)

      if (!is.null(error)) {
        report_error(error)
      }
      else {
        download_name <- basename(path)

        if (remove_query) download_name <- strsplit(download_name, "\\?")[[1]][1]
        destination_path <- file.path(temp_path, download_name)
        pin_log("Downloading ", path, " to ", destination_path)

        write_spec <- httr::write_disk(destination_path, overwrite = TRUE)
        result <- catch_error(httr::GET(path, write_spec, headers, config, http_utils_progress()))
        is_zip <- identical(result$headers$`content-type`, "application/zip")
        if (!is_zip && identical(result$headers$`content-type`, "application/octet-stream")) {
          is_zip <- file.size(destination_path) > 4 &&
            identical(readBin(destination_path, raw(), 4), as.raw(c(0x50, 0x4b, 0x03, 0x04)))
        }

        if (httr::http_error(result)) {
          error <- paste0(httr::http_status(result)$message, ". Failed to download remote file: ", path)
          report_error(error)
        }
      }
    }
  }

  if (!is.null(error)) {
    return()
  }

  new_cache <- old_pin$cache
  new_cache[[cache_index]] <- cache

  if (is_zip) {
    zip <- dir(temp_path, full.names = TRUE)
    pin_log("Extracting zip file '", zip, "'")
    zip::unzip(zip, exdir = temp_path)
    unlink(zip)
  }

  for (file in dir(temp_path, full.names = TRUE)) {
    file.copy(file, local_path, overwrite = TRUE, recursive = TRUE)
  }

  pin_registry_update(
    name = name,
    params = list(
      path = if (is.null(old_pin$path)) local_path else old_pin$path,
      cache = new_cache),
    component = component)

  local_path
}
