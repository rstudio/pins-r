#' Build a pin board from a vector of URLs
#'
#' @description
#' `board_url()` lets you build up a board from individual urls. This is
#' useful because [pin_download()] and [pin_get()] will be cached - they'll
#' only re-download the data if it's changed from the last time you downloaded
#' it (using the tools of
#' [HTTP caching](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching)).
#' You'll also be protected from the vagaries of the internet; if a fresh
#' download fails, you'll get the previously cached result with a warning.
#'
#' `board_url()` is read only and does not currently support versions.
#'
#' @param urls A named character vector of URLs If the URL ends in a `/`,
#'   `board_url` will look for a `data.txt` that provides metadata.
#' @inheritParams new_board
#' @export
#' @examples
#' github_raw <- "https://raw.githubusercontent.com/"
#' board <- board_url(c(
#'   files = paste0(github_raw, "rstudio/pins/master/tests/testthat/pin-files/"),
#'   rds = paste0(github_raw, "rstudio/pins/master/tests/testthat/pin-rds/"),
#'   raw = paste0(github_raw, "rstudio/pins/master/tests/testthat/pin-files/first.txt")
#' ))
#'
#' board %>% pin_read("rds")
#'
#' board %>% pin_download("files")
#' board %>% pin_download("raw")
board_url <- function(urls, cache = NULL) {
  if (!is.character(urls) || !is_named(urls)) {
    abort("`urls` must be a named character vector")
  }

  # Share cache across all instances of board_url(); pins are stored in
  # directories based on the hash of the URL to avoid cache collisions.
  cache <- cache %||% board_cache_path("local")

  new_board("pins_board_url",
    urls = urls,
    name = "url",
    cache = cache
  )
}

board_url_test <- function(urls, cache = tempfile()) {
  board_url(urls, cache = cache)
}

#' @export
pin_list.pins_board_url <- function(board, ...) {
  names(board$urls)
}

#' @export
board_pin_remove.pins_board_url <- function(board, name, ...) {
  abort("board_url() is read only")
}

#' @export
board_pin_upload.pins_board_url <- function(board, name, path, metadata,
                                              versioned = NULL, ...) {
  abort("board_url() is read only")
}

#' @export
board_pin_versions.pins_board_url <- function(board, name, ...) {
  abort("board_url() doesn't support versions")
}

#' @export
pin_meta.pins_board_url <- function(board, name, version = NULL, ...) {
  check_name(name)
  if (!has_name(board$urls, name)) {
    abort(glue("Can't find pin called {name}"))
  }
  if (!is.null(version)) {
    abort("board_url() doesn't support versions")
  }

  url <- board$urls[[name]]
  is_dir <- grepl("/$", url)

  pin_path <- fs::path(board$cache, hash(url))
  fs::dir_create(pin_path)

  if (is_dir) {
    # If directory, read from /data.txt
    download_cache(paste0(url, "data.txt"), pin_path, "data.txt")
    meta <- read_meta(pin_path)
    meta$url <- paste0(url, meta$file)
    meta$pin_path <- pin_path
    meta
  } else {
    # Otherwise assume it's a single file with no metadata
    list(
      type = "file",
      file = fs::path_file(url),
      url = url,
      pin_path = pin_path,
      api_version = 1
    )
  }
}

#' @export
pin_browse.pins_board_url <- function(board, name, version = NULL, ..., cache = FALSE) {
  meta <- pin_meta(board, name, version = version)
  if (cache) {
    browse_url(meta$pin_path)
  } else {
    browse_url(meta$url)
  }
}

#' @export
board_pin_download.pins_board_url <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  path <- map2_chr(meta$url, meta$file, ~ download_cache(.x, meta$pin_path, .y))

  list(
    meta = meta,
    dir = fs::path(board$cache, name),
    path = path
  )
}

# v0 ----------------------------------------------------------------------

#' @export
board_pin_create.pins_board_url <- function(board, path, name, metadata, ...) {
  abort("board_url() is read only")
}

#' @export
board_pin_get.pins_board_url <- function(board, name, version = NULL, ...) {
  abort(c(
    "`board_url()` doesn't support `pin()`",
    i = "Please use `pin_read()` instead"
  ))
}

# Helpers ------------------------------------------------------------------

download_cache <- function(url, path_dir, path_file) {
  cache_path <- download_cache_path(path_dir)
  cache <- read_cache(cache_path)[[url]]

  if (!is.null(cache)) {
    if (!has_expired(cache$expires)) {
      signal("", "pins_cache_cached")
      return(cache$path)
    }

    headers <- httr::add_headers(
      `If-Modified-Since` = http_date(cache$modified),
      `If-None-Match` = cache$etag
    )
  } else {
    headers <- NULL
  }

  path <- fs::path(path_dir, path_file)
  # only want to replace existing cache path if request is successful
  tmp_path <- tempfile()
  write_out <- httr::write_disk(tmp_path)

  req <- tryCatch(
    httr::GET(url, headers, write_out),
    error = function(e) {
      if (!is.null(cache)) {
        NULL
      } else {
        stop(e)
      }
    }
  )
  if (is.null(req)) {
    warn(glue::glue("Downloading '{path_file}' failed; falling back to cached version"))
    return(cache$path)
  }

  if (httr::status_code(req) <= 200) {
    signal("", "pins_cache_downloaded")
    fs::file_copy(tmp_path, path, overwrite = TRUE)
  } else if (httr::status_code(req) == 304) {
    signal("", "pins_cache_not_modified")
  } else {
    if (!is.null(cache)) {
      warn(glue::glue("Downloading '{path_file}' failed; falling back to cached version"))
      httr::warn_for_status(req)
      return(cache$path)
    } else {
      httr::stop_for_status(req)
    }
  }

  info <- httr::cache_info(req)
  if (info$cacheable) {
    update_cache(cache_path, url, list(
      expires = info$expires,
      etag = info$etag,
      modified = unclass(info$modified),
      path = path
    ))
  }

  path
}

download_cache_path <- function(path) {
  fs::path(path, "http-cache.yaml")
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
