pin_download <- function(path, name, component, ...) {
  must_cache <- identical(list(...)$cache, FALSE)

  old_pin <- tryCatch(pin_registry_retrieve(name, component), error = function(e) NULL)
  old_cache <- old_pin$cache

  report_error <- if (is.null(old_cache)) stop else warning

  cache <- list()
  cache$etag <- old_cache$etag
  cache$max_age <- if (!is.numeric(old_cache$max_age)) 0 else old_cache$max_age
  cache$change_age <- if (is.null(old_cache$change_age)) as.numeric(Sys.time()) - cache$max_age else old_cache$change_age
  cache$url <- path

  error <- NULL
  local_path <- old_pin$path

  pin_log("Checking 'change_age' header (time, change age, max age): ", as.numeric(Sys.time()), ", ", cache$change_age, ", ", cache$max_age)

  # skip downloading if max-age still valid
  if (as.numeric(Sys.time()) >= cache$change_age + cache$max_age || must_cache) {
    head_result <- httr::HEAD(path, httr::timeout(5))
    cache$etag <- head_result$headers$etag
    cache$max_age <- pin_file_cache_max_age(head_result$headers$`cache-control`)

    status <- tryCatch(httr::status_code(head_result), error = function(e) e$message)
    cache$change_age <- as.numeric(Sys.time())

    pin_log("Checking 'etag' (old, new): ", old_cache$etag, ", ", cache$etag)

    # skip downloading if etag has not changed
    if (is.null(old_cache) || is.null(old_cache$etag) || !identical(old_cache$etag, cache$etag) || must_cache) {
      if (is.character(status)) error <- paste0(status, ": ", path)
      if (status != 200) error <- paste0(status, " Failed to download remote file: ", path)

      if (!is.null(error)) {
        report_error(error)
      }
      else {
        local_path <- tempfile()
        dir.create(local_path)
        on.exit(unlink(local_path, recursive = TRUE))

        pin_log("Downloading: ", path)
        httr::GET(path, httr::write_disk(file.path(local_path, paste0("data", pin_file_extension(path))), overwrite = TRUE))
        on.exit(unlink(local_path))
      }
    }
  }

  if (is.null(error)) {
    # update change_age since we checked no change in HEAD
    pin_registry_update(name, "local", params = list(cache = cache))
  }

  if (is.null(local_path) || !file.exists(local_path)) {
    if (!is.null(local_path)) report_error("File does not exist: ", local_path)
    return()
  }

  final_path <- pin_registry_update(
    name = name,
    params = list(cache = cache),
    component = component)

  file.copy(dir(local_path, recursive = TRUE, full.names = TRUE) , final_path)

  final_path
}
