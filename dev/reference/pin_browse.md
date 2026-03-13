# Browse source of a pin

`pin_browse()` navigates you to the home of a pin, either on the
internet or on your local file system.

## Usage

``` r
pin_browse(board, name, version = NULL, local = FALSE)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  or another `board_` function.

- name:

  Pin name.

- version:

  Retrieve a specific version of a pin. Use
  [`pin_versions()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
  to find out which versions are available and when they were created.

- local:

  If `TRUE`, will open the local copy of the pin; otherwise will show
  you the home of the pin on the internet.

## Examples

``` r
board <- board_temp(versioned = TRUE)
board |> pin_write(1:10, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165843Z-8c3c9'
#> Writing to pin 'x'
board |> pin_write(1:11, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165843Z-7aa2f'
#> Writing to pin 'x'
board |> pin_write(1:12, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165843Z-70f41'
#> Writing to pin 'x'

board |> pin_browse("x", local = TRUE)
#> ℹ Pin at </tmp/RtmpxKttBg/pins-1e485da1bc9f/x/20260313T165843Z-8c3c9>
```
