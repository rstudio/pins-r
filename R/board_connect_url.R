#' Use a vector of Posit Connect vanity URLs as a board
#'
#' @description
#' `board_connect_url()` lets you build up a board from individual
#' [vanity urls](https://docs.posit.co/connect/user/content-settings/#custom-url).
#'
#' `board_connect_url()` is read only, and does not support versioning.
#'
#' @param vanity_urls A named character vector of
#'   [Connect vanity URLs](https://docs.posit.co/connect/user/content-settings/#custom-url),
#'   including trailing slash. This board is read only, and the best way to write to a pin 
#'   on Connect is [board_connect()].
#' @family boards
#' @inheritParams new_board
#' @inheritParams board_url
#' @inheritParams board_connect
#' @details
#' This board is a thin wrapper around [board_url()] which uses
#' `connect_auth_headers()` for authentication via environment variable.
#' @export
#' @examplesIf interactive()
#' connect_auth_headers()
#'
#' board <- board_connect_url(c(
#'     my_vanity_url_pin = "https://pub.current.posit.team/public/great-numbers/"
#' ))
#'
#' board %>% pin_read("my_vanity_url_pin")
#'
board_connect_url <- function(vanity_urls,
                              cache = NULL,
                              use_cache_on_failure = is_interactive(),
                              headers = connect_auth_headers()) {
  board_url(
    urls = vanity_urls,
    cache = cache,
    use_cache_on_failure = use_cache_on_failure,
    headers = headers
  )
}

#' @export
#' @rdname board_connect_url
connect_auth_headers <- function(key = Sys.getenv("CONNECT_API_KEY")) {
  c(Authorization = paste("Key", key))
}


vanity_url_test <- function(env = parent.frame()) {
  board <- board_connect_test()
  name <- pin_write(board, 1:10, random_pin_name())
  withr::defer(if (pin_exists(board, name)) pin_delete(board, name), env)

  vanity_slug <- ids::adjective_animal()
  body_path <- withr::local_tempfile()
  body <- list(force = FALSE, path = glue("/{vanity_slug}/"))
  jsonlite::write_json(body, body_path, auto_unbox = TRUE)
  body <- httr::upload_file(body_path, "application/json")

  meta <- pin_meta(board, name)
  path <- glue("v1/content/{meta$local$content_id}/vanity")
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "PUT", body_path)
  resp <- httr::PUT(board$url, path = path, body = body, auth)
  httr::stop_for_status(resp)

  glue("{board$url}/{vanity_slug}/")
}

board_connect_url_test <- function(...) {
  if (connect_has_ptd()) {
    board_connect_url_ptd(...)
  } else {
    board_connect_url_susan(...)
  }
}

board_connect_url_ptd <- function(...) {
  if (!connect_has_ptd()) {
    testthat::skip("board_connect_url_ptd() only works with Posit's demo server")
  }
  board_connect_url(..., cache = fs::file_temp())
}

board_connect_url_susan <- function(...) {
  creds <- read_creds()
  board_connect_url(
    ...,
    headers = connect_auth_headers(creds$susan_key)
  )
}
