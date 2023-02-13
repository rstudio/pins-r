#' Use a vector of Posit Connect vanity URLs as a board
#'
#' @description
#' `board_connect_url()` lets you build up a board from individual
#' [vanity urls](https://docs.posit.co/connect/user/content-settings/#custom-url).
#' To use a Posit Connect board like this one or [board_connect()], you need to
#' first authenticate. The easiest way to do so is by launching **Tools** -
#' **Global Options** - **Publishing** - **Connect**, and follow the
#' instructions.
#'
#' `board_connect_url()` is read only and does not support the use of a
#' [manifest file][write_board_manifest()].
#'
#' @param vanity_urls A named character vector of
#'   [Connect vanity URLs](https://docs.posit.co/connect/user/content-settings/#custom-url).
#'   This board is read only, and the best way to write to a pin on Connect is
#'   [board_connect()].
#' @family boards
#' @inheritParams new_board
#' @inheritParams board_connect
#' @export
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' board <- board_connect_url(c(
#'     my_vanity_url_pin = "https://colorado.posit.co/rsc/great-numbers/"
#' ))
#'
#' board %>% pin_read("my_vanity_url_pin")
#'
board_connect_url <- function(vanity_urls,
                              auth = c("auto", "manual", "envvar", "rsconnect"),
                              server = NULL,
                              account = NULL,
                              key = NULL,
                              cache = NULL,
                              use_cache_on_failure = is_interactive()) {

  server <- rsc_server(auth, server, account, key)
  cache <- cache %||% board_cache_path(paste0("connect-url-", hash(server$url)))

  board <- new_board_v1(
    "pins_board_connect_url",
    vanity_urls = vanity_urls,
    cache = cache,
    url = server$url,
    auth = server$auth,
    use_cache_on_failure = use_cache_on_failure
  )

  version <- try_catch_rsc_version(board, cache, server)
  pins_inform("Connecting to Posit Connect {version} at <{server$url}>")
  board
}

#' @export
pin_list.pins_board_connect_url <- function(board, ...) {
  names(board$vanity_urls)
}

#' @export
pin_exists.pins_board_connect_url <- function(board, name, ...) {
  name %in% names(board$vanity_urls)
}

#' @export
pin_meta.pins_board_connect_url <- function(board, name, version = NULL, ...) {
  check_name(name)
  check_pin_exists(board, name)

  url <- board$vanity_urls[[name]]

  cache_dir <- fs::path(board$cache, hash(url))
  fs::dir_create(cache_dir)

  if (grepl("/$", url)) {
    rsc_download(board, url, cache_dir, "data.txt")
    meta <- read_meta(cache_dir)
    local_meta(
      meta,
      name = name,
      dir = cache_dir,
      url = url,
      file_url = paste0(url, meta$file)
    )
  } else {
    cli::cli_abort(c(
      "Malformed vanity URL(s):",
      x = "{.url {url}}",
      i = "Check the vanity URL for your pin"
    ))
  }
}

#' @export
pin_fetch.pins_board_connect_url <- function(board, name, version = NULL, ...) {
  pin_fetch.pins_board_connect(board, name, version = NULL, ...)
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_connect_url <- function(board, ...) {
  ## TODO: add vanity URLs
  expr(board_connect_url(auth = "envvar"))
}

#' @rdname required_pkgs.pins_board
#' @export
required_pkgs.pins_board_connect_url <- function(x, ...) {
  ellipsis::check_dots_empty()
  "rsconnect"
}

# Unsupported features ----------------------------------------------------

#' @export
pin_delete.pins_board_connect_url <- function(board, names, ...) {
  abort_board_read_only("pins_board_connect_url")
}

#' @export
pin_store.pins_board_connect_url <- function(board, name, paths, metadata,
                                             versioned = NULL, ...) {
  abort_board_read_only("pins_board_connect_url")
}

#' @export
write_board_manifest_yaml.pins_board_connect_url <- function(board, manifest, ...) {
  abort_board_read_only("pins_board_connect_url")
}

# Testing setup -----------------------------------------------------------

board_connect_url_test_url <- function(env = parent.frame()) {
  board <- board_connect_test()
  name <- pin_write(board, 1:10, random_pin_name())
  withr::defer(if (pin_exists(board, name)) pin_delete(board, name), env)

  meta <- pin_meta(board, name)
  vanity_slug <- ids::adjective_animal()
  body <- glue('{{"force": false, "path": "/{vanity_slug}/"}}')
  connect_url <- glue("{board$url}/__api__/v1/content/{meta$local$content_id}/vanity")

  if (connect_has_colorado()) {
    auth <- paste("Key", Sys.getenv("CONNECT_API_KEY"))
  } else {
    creds <- read_creds()
    auth <- paste("Key", creds$susan_key)
  }

  result <- httr::PUT(connect_url, body = body, encode = "raw",
                      httr::add_headers(Authorization = auth))

  glue("{board$url}/{vanity_slug}/")
}

