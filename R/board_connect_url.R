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
#' board |> pin_read("my_vanity_url_pin")
#'
board_connect_url <- function(
  vanity_urls,
  cache = NULL,
  use_cache_on_failure = is_interactive(),
  headers = connect_auth_headers()
) {
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
