# Upgrading to pins 1.0.0

pins 1.0.0 introduced a completely new API and the old legacy API was
deprecated in pins 1.4.0, so now is a good time to switch to the new
interface. This vignette shows a couple of examples of updating legacy
code to the modern API, then provides a full set of equivalences between
the legacy and modern function names.

``` r

library(pins)
```

## Examples

A simple example of the legacy API looks something like this:

``` r

# Legacy API
board_register_local("vignette", tempfile())

pin(head(mtcars), "mtcars", board = "vignette")
pin_get("mtcars", board = "vignette")
```

To convert to the modern API you need to make two major changes:

- Instead of registering a named board, you create an explicit board
  object.
- You use
  [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  instead of
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) and
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md).

``` r

# Modern API
board <- board_local()

pin_write(board, head(mtcars), "mtcars")
pin_read(board, "mtcars")
```

Since the board object is always the first argument, you might also want
to use the pipe:

``` r

# Modern API
board <- board_local()

board |> pin_write(head(mtcars), "mtcars")
board |> pin_read("mtcars")
```

### Pinning files

Another way to use
[`pin()`](https://pins.rstudio.com/dev/reference/pin.md) is with a path
to a file:

``` r

# Legacy API
path <- tempfile()
writeLines(letters, path)

pin(path, "alphabet", board = "vignette")
pin_get("alphabet", board = "vignette")
```

pins 1.0.0 clearly separates the two cases of pin an object and pinning
a file, so here instead of
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) you
need to
[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
and
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md):

``` r

# Modern API
board |> pin_upload(path, "alphabet")
board |> pin_download("alphabet")
```

### Pinning a url

Finally, you can
[`pin()`](https://pins.rstudio.com/dev/reference/pin.md) a url to
automatically re-download it when it changes:

``` r

# Legacy API
base <- "https://raw.githubusercontent.com/rstudio/pins-r/main/tests/testthat/"

(pin(paste0(base, "pin-files/first.txt"), board = "vignette"))
```

This now needs to be made explicit with the new
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md),
and since this returns a path, not a file, you need to use
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md):

``` r

# Modern API
board_github <- board_url(c(
  raw = paste0(base, "pin-files/first.txt")
))
board_github |> pin_download("raw")
```

### Implicit board

It’s also possible to use
[`pin()`](https://pins.rstudio.com/dev/reference/pin.md) and
[`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) without
an explicit board argument, in which case it automatically uses a local
board:

``` r

# Legacy API
pin(data.frame(x = 1:3), "test-data")
pin_get("test-data")
```

To convert this code, you need to create an explicit
[`board_local()`](https://pins.rstudio.com/dev/reference/board_folder.md):

``` r

# Modern API
board <- board_local()

board |> pin_write(data.frame(x = 1:3), "test-data")
board |> pin_read("test-data")
```

## Equivalents

### Board functions

| Legacy API | Modern API |
|----|----|
| [`board_register_azure()`](https://pins.rstudio.com/dev/reference/legacy_azure.md) | [`board_azure()`](https://pins.rstudio.com/dev/reference/board_azure.md) |
| [`board_register_datatxt()`](https://pins.rstudio.com/dev/reference/legacy_datatxt.md) | Not currently implemented |
| [`board_register_dospace()`](https://pins.rstudio.com/dev/reference/legacy_dospace.md) | Not currently implemented |
| [`board_register_gcloud()`](https://pins.rstudio.com/dev/reference/legacy_gcloud.md) | [`board_gcs()`](https://pins.rstudio.com/dev/reference/board_gcs.md) |
| [`board_register_github()`](https://pins.rstudio.com/dev/reference/legacy_github.md) | Use [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md) together with [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) |
| [`board_register_local()`](https://pins.rstudio.com/dev/reference/legacy_local.md) | [`board_local()`](https://pins.rstudio.com/dev/reference/board_folder.md) |
| [`board_register_kaggle()`](https://pins.rstudio.com/dev/reference/legacy_kaggle.md) | [`board_kaggle_dataset()`](https://pins.rstudio.com/dev/reference/board_kaggle.md) / `board_kaggle_competition()` |
| [`board_register_rsconnect()`](https://pins.rstudio.com/dev/reference/board_register.md) | [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md) |
| [`board_register_s3()`](https://pins.rstudio.com/dev/reference/legacy_s3.md) | [`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md) |
| [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) with a URL | [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) |

Future releases will add support for additional boards based on user
feedback.

### Pin functions

| Legacy API | Modern API |
|----|----|
| [`board_browse()`](https://pins.rstudio.com/dev/reference/custom-boards.md) | [`pin_browse()`](https://pins.rstudio.com/dev/reference/pin_browse.md) |
| [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) | [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) / [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md) |
| [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) | [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) / [`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md) |
| [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) | [`pin_search()`](https://pins.rstudio.com/dev/reference/pin_search.md) |
| [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md) | [`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md) |
| [`pin_reactive()`](https://pins.rstudio.com/dev/reference/pin_reactive.md) | [`pin_reactive_read()`](https://pins.rstudio.com/dev/reference/pin_reactive_read.md) / [`pin_reactive_download()`](https://pins.rstudio.com/dev/reference/pin_reactive_read.md) |
| [`pin_remove()`](https://pins.rstudio.com/dev/reference/pin_remove.md) | [`pin_delete()`](https://pins.rstudio.com/dev/reference/pin_delete.md) |
