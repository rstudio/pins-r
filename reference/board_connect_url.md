# Use a vector of Posit Connect vanity URLs as a board

`board_connect_url()` lets you build up a board from individual [vanity
urls](https://docs.posit.co/connect/user/content-settings/#custom-url).

`board_connect_url()` is read only, and does not support versioning.

## Usage

``` r
board_connect_url(
  vanity_urls,
  cache = NULL,
  use_cache_on_failure = is_interactive(),
  headers = connect_auth_headers()
)

connect_auth_headers(key = Sys.getenv("CONNECT_API_KEY"))
```

## Arguments

- vanity_urls:

  A named character vector of [Connect vanity
  URLs](https://docs.posit.co/connect/user/content-settings/#custom-url),
  including trailing slash. This board is read only, and the best way to
  write to a pin on Connect is
  [`board_connect()`](https://pins.rstudio.com/reference/board_connect.md).

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- use_cache_on_failure:

  If the pin fails to download, is it ok to use the last cached version?
  Defaults to `is_interactive()` so you'll be robust to poor internet
  connectivity when exploring interactively, but you'll get clear errors
  when the code is deployed.

- headers:

  Named character vector for additional HTTP headers (such as for
  authentication). See `connect_auth_headers()` for Posit Connect
  support.

- key:

  The Posit Connect API key.

## Details

This board is a thin wrapper around
[`board_url()`](https://pins.rstudio.com/reference/board_url.md) which
uses `connect_auth_headers()` for authentication via environment
variable.

## See also

Other boards:
[`board_connect()`](https://pins.rstudio.com/reference/board_connect.md),
[`board_folder()`](https://pins.rstudio.com/reference/board_folder.md),
[`board_url()`](https://pins.rstudio.com/reference/board_url.md)

## Examples

``` r
if (FALSE) { # interactive()
connect_auth_headers()

board <- board_connect_url(c(
    my_vanity_url_pin = "https://pub.current.posit.team/public/great-numbers/"
))

board |> pin_read("my_vanity_url_pin")
}
```
