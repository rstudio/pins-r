#' Use a vector of URLs as a board
#'
#' @description
#' `board_url()` lets you build up a board from individual urls. This is
#' useful because [pin_download()] and [pin_read()] will be cached - they'll
#' only re-download the data if it's changed from the last time you downloaded
#' it (using the tools of
#' [HTTP caching](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching)).
#' You'll also be protected from the vagaries of the internet; if a fresh
#' download fails, you'll get the previously cached result with a warning.
#'
#' `board_url()` is read only and does not currently support versions.
#'
#' @param urls identify available pins, possible values:
#'   - unnamed character scalar, i.e. single string: URL to a manifest file.
#'     If the URL ends in a `/`, `board_url()` will look for a `pins.txt`
#'     containing the manifest. If `pins.txt` parses to a named list,
#'     versioning is supported. If it parses to a named character vector,
#'     the board will not support versioning.
#'   - named list, values are character vectors of URLs, each element of the
#'     vector refers to a version of the particular pin. If a URL ends in a `/`,
#'     `board_url()` will look for a `data.txt` that provides metadata.
#'   - named character vector of URLs: If the URL ends in a `/`,
#'     `board_url()` will look for a `data.txt` that provides metadata. The
#'     easiest way to generate this file is to upload a pin directory created by
#'     [board_folder()]. Versioning is not supported.
#' @param use_cache_on_failure If the pin fails to download, is it ok to
#'   use the last cached version? Defaults to `is_interactive()` so you'll
#'   be robust to poor internet connectivity when exploring interactively,
#'   but you'll get clear errors when the code is deployed.
#' @family boards
#' @inheritParams new_board
#' @export
#' @examples
#' github_raw <- "https://raw.githubusercontent.com/"
#' board <- board_url(c(
#'   files = paste0(github_raw, "rstudio/pins-r/master/tests/testthat/pin-files/"),
#'   rds = paste0(github_raw, "rstudio/pins-r/master/tests/testthat/pin-rds/"),
#'   raw = paste0(github_raw, "rstudio/pins-r/master/tests/testthat/pin-files/first.txt")
#' ))
#'
#' board %>% pin_read("rds")
#' board %>% pin_browse("rds", local = TRUE)
#'
#' board %>% pin_download("files")
#' board %>% pin_download("raw")
board_url <- function(urls, cache = NULL, use_cache_on_failure = is_interactive()) {

  if (is_scalar_character(urls) && !is_named(urls)) {
    # single URL

    # download, parse, then call again using list, or named character vector

  } else if (is_list(urls) && is_named(urls) && all(map_lgl(urls, is_character))) {
    # named list of URLs
    versioned = TRUE
  } else if (is.character(urls) && is_named(urls)) {
    # unnamed character vector of URLs
    versioned = FALSE
  } else {
    # unsupported
    # TODO: make more-helpful error
    abort(
      "`urls`: must be single string, named list, or named character vector"
    )
  }

  # Share cache across all instances of board_url(); pins are stored in
  # directories based on the hash of the URL to avoid cache collisions.
  cache <- cache %||% board_cache_path("url")

  new_board_v1("pins_board_url",
    urls = urls,
    cache = cache,
    versioned = versioned,
    use_cache_on_failure = use_cache_on_failure
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
pin_exists.pins_board_url <- function(board, name, ...) {
  name %in% names(board$urls)
}

#' @export
pin_meta.pins_board_url <- function(board, name, version = NULL, ...) {
  check_name(name)
  check_pin_exists(board, name)

  # check if we need this
  if (!is.null(version) && !board$versioned) {
    abort("this board_url() is not versioned")
  }

  url <- board$urls[[name]]
  is_dir <- grepl("/$", url)

  cache_dir <- fs::path(board$cache, hash(url))
  fs::dir_create(cache_dir)

  if (is_dir) {
    # If directory, read from /data.txt
    http_download(
      url = paste0(url, "data.txt"),
      path_dir = cache_dir,
      path_file = "data.txt",
      use_cache_on_failure = board$use_cache_on_failure
    )
    meta <- read_meta(cache_dir)
    local_meta(meta,
      name = name,
      dir = cache_dir,
      url = url,
      file_url = paste0(url, meta$file)
    )
  } else {
    # Otherwise assume it's a single file with no metadata
    meta <- list(
      type = "file",
      file = fs::path_file(url),
      api_version = 1
    )
    local_meta(meta,
      name = name,
      dir = cache_dir,
      url = url,
      file_url = url
    )
  }
}

#' @export
pin_fetch.pins_board_url <- function(board, name, version = NULL, ...) {
  meta <- pin_meta(board, name, version = version)
  cache_touch(board, meta)

  path <- map2_chr(meta$local$file_url, meta$file, function(url, file) {
    http_download(
      url = url,
      path_dir = meta$local$dir,
      path_file = file,
      use_cache_on_failure = board$use_cache_on_failure
    )
  })

  meta
}

#' @export
pin_versions.pins_board_url <- function(board, name, ...) {

  if (!board$versioned) {
    abort("This board_url() is not versioned")
  }

  check_name(name)
  check_pin_exists(board, name)

  paths <- board$urls[[name]]
  version_from_path(fs::path_file(paths)) # works for URLs too
}

# Unsupported features ----------------------------------------------------

#' @export
pin_delete.pins_board_url <- function(board, names, ...) {
  abort_board_read_only("board_url")
}

#' @export
pin_store.pins_board_url <- function(board, name, paths, metadata,
                                              versioned = NULL, ...) {
  abort_board_read_only("board_url")
}

# Helpers ------------------------------------------------------------------
http_download <- function(url, path_dir, path_file, ...,
                          use_cache_on_failure = FALSE,
                          on_failure = NULL) {
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
    httr::GET(url, headers, ..., write_out),
    error = function(e) {
      if (!is.null(cache) && use_cache_on_failure) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  if (is.null(req)) {
    warn(glue::glue("Downloading '{path_file}' failed; falling back to cached version"))
    cache$path
  } else if (httr::status_code(req) <= 200) {
    signal("", "pins_cache_downloaded")
    if (fs::file_exists(path)) fs::file_chmod(path, "u+w")
    fs::file_copy(tmp_path, path, overwrite = TRUE)
    fs::file_chmod(path, "u=r")

    info <- httr::cache_info(req)
    if (info$cacheable) {
      update_cache(cache_path, url, list(
        expires = info$expires,
        etag = info$etag,
        modified = unclass(info$modified),
        path = path
      ))
    } else {
      cli::cli_alert("{.url {url}} is not cacheable")
    }

    path
  } else if (httr::status_code(req) == 304) {
    signal("", "pins_cache_not_modified")
    cache$path
  } else {
    if (!is.null(cache) && use_cache_on_failure) {
      warn(glue::glue("Downloading '{path_file}' failed; falling back to cached version"))
      httr::warn_for_status(req)
      cache$path
    } else {
      if (is.null(on_failure)) {
        httr::stop_for_status(req)
      } else {
        on_failure(req)
      }
    }
  }
}

download_cache_path <- function(path) {
  fs::path(path, "download-cache.yaml")
}

has_expired <- function(x) {
  if (is.null(x)) {
    TRUE
  } else {
    unclass(Sys.time()) > x
  }
}

http_date <- function(x = Sys.time(), tz = "UTC") {
  if (is.null(x)) {
    return(NULL)
  }

  withr::local_locale(LC_TIME = "C")
  strftime(.POSIXct(x), "%a, %d %b %Y %H:%M:%S", tz = tz, usetz = TRUE)
}
