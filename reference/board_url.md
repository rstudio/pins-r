# Use a vector of URLs as a board

`board_url()` lets you build up a board from individual urls or a
[manifest
file](https://pins.rstudio.com/reference/write_board_manifest.md).

`board_url()` is read only.

## Usage

``` r
board_url(
  urls,
  cache = NULL,
  use_cache_on_failure = is_interactive(),
  headers = NULL
)
```

## Arguments

- urls:

  Identify available pins being served at a URL or set of URLs (see
  details):

  - Unnamed string: URL to a [manifest
    file](https://pins.rstudio.com/reference/write_board_manifest.md).

  - Named character vector: URLs to specific pins (does not support
    versioning).

  - Named list: URLs to pin version directories (supports versioning).

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
  authentication). See
  [`connect_auth_headers()`](https://pins.rstudio.com/reference/board_connect_url.md)
  for Posit Connect support.

## Details

The way `board_url()` works depends on the type of the `urls` argument:

- Unnamed character scalar, i.e. **a single URL** to a [manifest
  file](https://pins.rstudio.com/reference/write_board_manifest.md): If
  the URL ends in a `/`, `board_url()` will look for a `_pins.yaml`
  manifest. If the manifest file parses to a named list, versioning is
  supported. If it parses to a named character vector, the board will
  not support versioning.

- **Named character vector of URLs**: If the URLs end in a `/`,
  `board_url()` will look for a `data.txt` that provides metadata for
  the associated pin. The easiest way to generate this file is to upload
  a pin version directory created by
  [`board_folder()`](https://pins.rstudio.com/reference/board_folder.md).
  Versioning is not supported.

- **Named list**, where the values are character vectors of URLs and
  each element of the vector refers to a version of the particular pin:
  If a URL ends in a `/`, `board_url()` will look for a `data.txt` that
  provides metadata. Versioning is supported.

Using a vector of URLs can be useful because
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
and [`pin_read()`](https://pins.rstudio.com/reference/pin_read.md) will
be cached; they'll only re-download the data if it's changed from the
last time you downloaded it (using the tools of [HTTP
caching](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching)).
You'll also be protected from the vagaries of the internet; if a fresh
download fails, you'll get the previously cached result with a warning.

Using a [manifest
file](https://pins.rstudio.com/reference/write_board_manifest.md) can be
useful because you can serve a board of pins and allow collaborators to
access the board straight from a URL, without worrying about board-level
storage details. Some examples are provided in
[`vignette("using-board-url")`](https://pins.rstudio.com/articles/using-board-url.md).

## Authentication for `board_url()`

The `headers` argument allows you to pass authentication details or
other HTTP headers to the board, such as for a Posit Connect vanity URL
that is not public (see
[`board_connect_url()`](https://pins.rstudio.com/reference/board_connect_url.md))
or a private GitHub repo.

    gh_pat_auth <- c(
      Authorization = paste("token", "github_pat_XXXX")
    )
    board <- board_url(
      "https://raw.githubusercontent.com/username/repo/main/path/to/pins",
      headers = gh_pat_auth
    )

    board |> pin_list()

## See also

Other boards:
[`board_connect()`](https://pins.rstudio.com/reference/board_connect.md),
[`board_connect_url()`](https://pins.rstudio.com/reference/board_connect_url.md),
[`board_folder()`](https://pins.rstudio.com/reference/board_folder.md)

## Examples

``` r
github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)

## with a named vector of URLs to specific pins:
b1 <- board_url(c(
  files = github_raw("rstudio/pins-r/main/tests/testthat/pin-files/"),
  rds = github_raw("rstudio/pins-r/main/tests/testthat/pin-rds/"),
  raw = github_raw("rstudio/pins-r/main/tests/testthat/pin-files/first.txt")
))

b1 |> pin_read("rds")
#>     x
#> 1   1
#> 2   2
#> 3   3
#> 4   4
#> 5   5
#> 6   6
#> 7   7
#> 8   8
#> 9   9
#> 10 10
b1 |> pin_browse("rds", local = TRUE)
#> ℹ Pin at <~/.cache/pins/url/5e3ed054e0c263a996340d449a12324b>

b1 |> pin_download("files")
#> [1] "~/.cache/pins/url/6a19374ae8fd88eaccc9cbd6e3022001/first.txt" 
#> [2] "~/.cache/pins/url/6a19374ae8fd88eaccc9cbd6e3022001/second.txt"
b1 |> pin_download("raw")
#> [1] "~/.cache/pins/url/4bb0af0abae87c78f4a1b6fbe7e2642c/first.txt"

## with a manifest file:
b2 <- board_url(github_raw("rstudio/pins-r/main/tests/testthat/pin-board/"))
b2 |> pin_list()
#> [1] "x" "y"
b2 |> pin_versions("y")
#> # A tibble: 2 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20221215T180357Z-9ae7a 2022-12-15 18:03:57 9ae7a
#> 2 20221215T180400Z-b81d5 2022-12-15 18:04:00 b81d5
```
