pin_download <- function(path, name, component, ...) {
  must_cache <- identical(list(...)$cache, FALSE)
  headers <- list(...)$headers
  config <- list(...)$config
  custom_etag <- list(...)$custom_etag
  remove_query <- identical(list(...)$remove_query, TRUE)

  local_path <- pin_registry_path(component, name)

  old_pin <- tryCatch(pin_registry_retrieve(name, component), error = function(e) NULL)
  old_cache <- old_pin$cache

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
    }
  }

  report_error <- if (is.null(old_cache)) stop else warning

  cache <- list()
  cache$etag <- old_cache$etag
  cache$max_age <- if (!is.numeric(old_cache$max_age)) 0 else old_cache$max_age
  cache$change_age <- if (is.null(old_cache$change_age)) as.numeric(Sys.time()) - cache$max_age else old_cache$change_age
  cache$url <- path

  error <- NULL
  is_zip <- FALSE

  pin_log("Checking 'change_age' header (time, change age, max age): ", as.numeric(Sys.time()), ", ", cache$change_age, ", ", cache$max_age)

  # skip downloading if max-age still valid
  if (as.numeric(Sys.time()) >= cache$change_age + cache$max_age || must_cache) {

    status <- 200
    if (!is.null(custom_etag)) {
      pin_log("Using custom 'etag' (old, new): ", old_cache$etag, ", ", custom_etag)
      cache$etag <- custom_etag
    }
    else {
      head_result <- httr::HEAD(path, httr::timeout(5), headers, config)
      cache$etag <- head_result$headers$etag
      cache$max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

      status <- tryCatch(httr::status_code(head_result), error = function(e) e$message)
      cache$change_age <- as.numeric(Sys.time())

      pin_log("Checking 'etag' (old, new): ", old_cache$etag, ", ", cache$etag)
    }

    # skip downloading if etag has not changed
    if (is.null(old_cache) || is.null(old_cache$etag) || !identical(old_cache$etag, cache$etag) || must_cache) {
      if (is.character(status)) error <- paste0(status, ": ", path)
      if (status != 200) error <- paste0(status, " Failed to download remote file: ", path)

      if (!is.null(error)) {
        report_error(error)
      }
      else {
        download_name <- basename(path)

        if (remove_query) download_name <- strsplit(download_name, "\\?")[[1]][1]
        destination_path <- file.path(local_path, download_name)
        pin_log("Downloading ", path, " to ", destination_path)

        write_spec <- httr::write_disk(destination_path, overwrite = TRUE)
        result <- httr::GET(path, write_spec, headers, config)
        is_zip <- identical(result$headers$`content-type`, "application/zip")

        if (httr::status_code(result) != 200) {
          error <- paste0(status, " Failed to download remote file: ", path)
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

  pin_registry_update(
    name = name,
    params = list(
      path = local_path,
      source = cache$url,
      cache = new_cache),
    component = component)

  if (is_zip) {
    zip <- dir(local_path, full.names = TRUE)
    utils::unzip(zip, exdir = local_path)
    unlink(zip)
  }

  local_path
}
