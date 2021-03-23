
download_cache <- function(board, url, path, use_cache = TRUE) {
  cache_meta <- cache_read_metadata(board)
  cache <- cache_meta[[url]]

  cache_path <- fs::path(board$cache, cache$path %||% path)
  if (use_cache && !is.null(cache)) {
    if (!has_expired(cache$expires)) {
      signal("", "pins_cache_cached")
      return(cache_path)
    }

    headers <- httr::add_headers(
      `If-Modified-Since` = http_date(cache$modified),
      `If-None-Match` = cache$etag
    )
  } else {
    headers <- NULL
  }

  # only want to replace existing cache path if request is successful
  tmp_path <- tempfile()
  write_out <- httr::write_disk(tmp_path)
  req <- httr::GET(url, headers, write_out)

  if (httr::status_code(req) <= 200) {
    fs::file_copy(tmp_path, cache_path, overwrite = TRUE)
  } else if (httr::status_code(req) == 304) {
    signal("", "pins_cache_not_modified")
  } else {
    httr::stop_for_status(req)
  }

  info <- httr::cache_info(req)
  if (info$cacheable) {
    cache_meta[[url]] <- list(
      expires = info$expires,
      etag = info$etag,
      modified = unclass(info$modified),
      path = path
    )
    cache_write_metadata(cache_meta, board)
  }

  cache_path
}

cache_read_metadata <- function(board) {
  path <- fs::path(board$cache, "cache.yaml")
  if (!fs::file_exists(path)) {
    list()
  } else {
    yaml::read_yaml(path, eval.expr = FALSE)
  }
}

cache_write_metadata <- function(data, board) {
  path <- fs::path(board$cache, "cache.yaml")
  yaml::write_yaml(data, path)
}

has_expired <- function(x) {
  if (is.null(x)) {
    TRUE
  } else {
    unclass(Sys.time()) > x
  }
}

http_date <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  withr::local_locale(LC_TIME = "C")
  strftime(.POSIXct(x), "%a, %d %b %Y %H:%M:%S", tz = "UTC", usetz = TRUE)
}
