#' Use a vector of URLs as a board
#'
#' @description
#' `board_url()` lets you build up a board from individual urls or a [manifest
#' file][write_board_manifest()].
#'
#' `board_url()` is read only.
#'
#' @param urls Identify available pins being served at a URL or set of URLs (see details):
#'   - Unnamed string: URL to a [manifest file][write_board_manifest()].
#'   - Named character vector: URLs to specific pins (does not support versioning).
#'   - Named list: URLs to pin version directories (supports versioning).
#' @param use_cache_on_failure If the pin fails to download, is it ok to
#'   use the last cached version? Defaults to `is_interactive()` so you'll
#'   be robust to poor internet connectivity when exploring interactively,
#'   but you'll get clear errors when the code is deployed.
#' @param headers Named character vector for additional HTTP headers (such as for
#'   authentication). See [connect_auth_headers()] for Posit Connect support.
#' @family boards
#' @inheritParams new_board
#' @details
#' The way `board_url()` works depends on the type of the `urls` argument:
#'   - Unnamed character scalar, i.e. **a single URL** to a
#'     [manifest file][write_board_manifest()]: If the URL ends in a `/`,
#'     `board_url()` will look for a `_pins.yaml` manifest. If the manifest
#'     file parses to a named list, versioning is supported. If it parses to a
#'     named character vector, the board will not support versioning.
#'   - **Named character vector of URLs**: If the URLs end in a `/`,
#'     `board_url()` will look for a `data.txt` that provides metadata for the
#'     associated pin. The easiest way to generate this file is to upload a pin
#'     version directory created by [board_folder()]. Versioning is not supported.
#'   - **Named list**, where the values are character vectors of URLs and each
#'     element of the vector refers to a version of the particular pin: If a
#'     URL ends in a `/`, `board_url()` will look for a `data.txt` that
#'     provides metadata. Versioning is supported.
#'
#' Using a vector of URLs can be useful because [pin_download()] and
#' [pin_read()] will be cached; they'll only re-download the data if it's
#' changed from the last time you downloaded it (using the tools of
#' [HTTP caching](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching)).
#' You'll also be protected from the vagaries of the internet; if a fresh
#' download fails, you'll get the previously cached result with a warning.
#'
#' Using a [manifest file][write_board_manifest()] can be useful because you
#' can serve a board of pins and allow collaborators to access the board
#' straight from a URL, without worrying about board-level storage details.
#' Some examples are provided in `vignette("using-board-url")`.
#'
#' # Authentication for `board_url()`
#'
#' The `headers` argument allows you to pass authentication details or other
#' HTTP headers to the board, such as for a Posit Connect vanity URL that is
#' not public (see [board_connect_url()]) or a private GitHub repo.
#'
#' ```r
#' gh_pat_auth <- c(
#'   Authorization = paste("token", "github_pat_XXXX")
#' )
#' board <- board_url(
#'   "https://raw.githubusercontent.com/username/repo/main/path/to/pins",
#'   headers = gh_pat_auth
#' )
#'
#' board %>% pin_list()
#' ```
#'
#' @export
#' @examplesIf !pins:::is_cran_check()
#' github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)
#'
#' ## with a named vector of URLs to specific pins:
#' b1 <- board_url(c(
#'   files = github_raw("rstudio/pins-r/main/tests/testthat/pin-files/"),
#'   rds = github_raw("rstudio/pins-r/main/tests/testthat/pin-rds/"),
#'   raw = github_raw("rstudio/pins-r/main/tests/testthat/pin-files/first.txt")
#' ))
#'
#' b1 %>% pin_read("rds")
#' b1 %>% pin_browse("rds", local = TRUE)
#'
#' b1 %>% pin_download("files")
#' b1 %>% pin_download("raw")
#'
#' ## with a manifest file:
#' b2 <- board_url(github_raw("rstudio/pins-r/main/tests/testthat/pin-board/"))
#' b2 %>% pin_list()
#' b2 %>% pin_versions("y")
#'
board_url <- function(urls,
                      cache = NULL,
                      use_cache_on_failure = is_interactive(),
                      headers = NULL) {

  check_headers(headers)
  url_format <- get_url_format(urls)
  if (url_format == "pins_yaml") {
    manifest <- get_manifest(urls, headers)
    board <- board_url(
      manifest,
      cache = cache,
      use_cache_on_failure = use_cache_on_failure,
      headers = headers
    )
    return(board)
  }
  versioned <- url_format == "manifest_content"

  # Share cache across all instances of board_url(); pins are stored in
  # directories based on the hash of the URL to avoid cache collisions.
  cache <- cache %||% board_cache_path("url")

  new_board_v1(
    "pins_board_url",
    urls = urls,
    cache = cache,
    versioned = versioned,
    use_cache_on_failure = use_cache_on_failure,
    headers = headers
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
  check_pin_name(name)
  check_pin_exists(board, name)
  if (!is.null(version) && !board$versioned) {
    abort_board_not_versioned("board_url")
  }

  url <- board$urls[[name]]

  if (board$versioned) {
    versions <- pin_versions(board, name)$version
    version <- check_pin_version(board, name, version)
    url <- board$urls[[name]][versions == version]
  }

  is_dir <- grepl("/$", url)
  cache_dir <- fs::path(board$cache, hash(url))
  fs::dir_create(cache_dir)

  if (is_dir) {
    # If directory, read from /data.txt
    http_download(
      url = paste0(url, "data.txt"),
      path_dir = cache_dir,
      path_file = "data.txt",
      use_cache_on_failure = board$use_cache_on_failure,
      headers = board$headers
    )
    meta <- read_meta(cache_dir)
    local_meta(
      meta,
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
    local_meta(
      meta,
      name = name,
      dir = cache_dir,
      url = url,
      file_url = url
    )
  }
}

#' @export
pin_versions.pins_board_url <- function(board, name, ...) {

  if (!board$versioned) {
    abort_board_not_versioned("board_url")
  }

  check_pin_name(name)
  check_pin_exists(board, name)

  paths <- board$urls[[name]]
  version_from_path(fs::path_file(paths))
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
      use_cache_on_failure = board$use_cache_on_failure,
      headers = board$headers
    )
  })

  meta
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_url <- function(board, ...) {
  urls <- check_board_deparse(board, "urls")
  expr(board_url(!!urls))
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

#' @export
pin_version_delete.pins_board_url <- function(board, name, version, ...) {
  abort_board_read_only("board_url")
}

#' @export
write_board_manifest_yaml.pins_board_url <- function(board, manifest, ...) {
  abort_board_read_only("board_url")
}

# Helpers ------------------------------------------------------------------

get_url_format <- function(urls) {
  if (is_scalar_character(urls) && !is_named(urls)) {
    "pins_yaml"
  } else if (is_list(urls) && is_named(urls) && all(map_lgl(urls, is_character))) {
    "manifest_content"
  } else if (is.character(urls) && is_named(urls)) {
    "vector_of_urls"
  } else {
    cli_abort(
      c(
        "{.var urls} must resolve to either:",
        "*" = "an unnamed character scalar, i.e. a single URL",
        "*" = "a named character vector",
        "*" = "a named list, where all elements are character scalars or vectors"
      ),
      class = "pins_error_board_url_argument",
      urls = urls
    )
  }
}

get_manifest <- function(url, headers, call = rlang::caller_env()) {
  # if ends with "/", look for manifest
  if (grepl("/$", url)) {
    url <- paste0(url, manifest_pin_yaml_filename)
  }

  # if request fails or returns with error code
  tryCatch(
    {
      resp <- httr::GET(url, httr::add_headers(headers))
      httr::stop_for_status(resp)
    },
    error = function(e) {
      cli_abort(
        message = "Failed to access manifest file at {.url {url}}:",
        class = "pins_error_board_url_request",
        parent = e,
        url = url,
        call = call
      )
    }
  )

  # if file is not parsable
  tryCatch(
    {
      text <- httr::content(resp, as = "text", encoding = "UTF-8")
      manifest <- yaml::yaml.load(text)
    },
    error = function(e) {
      cli_abort(
        message = c(
          "Failed to parse manifest file at URL {.url {url}}:",
          " " = "{e$message}",
          "i" = "Manifest file must be text and parsable as YAML."
        ),
        class = "pins_error_board_url_parse",
        parent = e,
        resp = resp,
        call = call
      )
    }
  )

  # url_root is directory containing manifest-file
  url_root <- sub("[^/]*$", "", url)
  # for each manifest entry, prepend url_root to each path entry
  manifest <- map(manifest, ~ paste0(url_root, .x))
  manifest
}

http_download <- function(url, path_dir, path_file, ...,
                          use_cache_on_failure = FALSE,
                          headers = NULL,
                          on_failure = NULL) {
  cache_path <- download_cache_path(path_dir)
  cache <- read_cache(cache_path)[[url]]

  if (!is.null(cache)) {
    if (!has_expired(cache$expires)) {
      signal("", "pins_cache_cached")
      return(cache$path)
    }

    headers <- c(
      headers,
      `If-Modified-Since` = http_date(cache$modified),
      `If-None-Match` = cache$etag
    )
  }

  path <- fs::path(path_dir, path_file)
  # only want to replace existing cache path if request is successful
  tmp_path <- tempfile()
  write_out <- httr::write_disk(tmp_path)

  req <- tryCatch(
    httr::GET(url, httr::add_headers(headers), ..., write_out),
    error = function(e) {
      if (!is.null(cache) && use_cache_on_failure) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  if (is.null(req)) {
    warn(glue("Downloading '{path_file}' failed; falling back to cached version"))
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
      warn(glue("Downloading '{path_file}' failed; falling back to cached version"))
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

read_cache <- function(path) {
  if (file.exists(path)) {
    yaml::read_yaml(path, eval.expr = FALSE)
  } else {
    list()
  }
}

update_cache <- function(path, key, value) {
  cache <- read_cache(path)
  cache[[key]] <- value
  write_yaml(cache, path)

  value
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

check_headers <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.null(x) && (!is_character(x) || !is_named(x))) {
    stop_input_type(x, "a named character vector", allow_null = TRUE, arg = arg, call = call)
  }

}
