# List all pins

List names of all pins in a board. This is a low-level function; use
[`pin_search()`](https://pins.rstudio.com/dev/reference/pin_search.md)
to get more data about each pin in a convenient form.

## Usage

``` r
pin_list(board, ...)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  or another `board_` function.

- ...:

  Other arguments passed on to methods

## Value

A character vector

## Examples

``` r
board <- board_temp()

board |> pin_write(1:5, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20260802T194603Z-1d21e'
#> Writing to pin 'x'
board |> pin_write(letters, "y")
#> Guessing `type = 'rds'`
#> Creating new version '20260802T194603Z-be07d'
#> Writing to pin 'y'
board |> pin_write(runif(20), "z")
#> Guessing `type = 'rds'`
#> Creating new version '20260802T194603Z-cdad0'
#> Writing to pin 'z'

board |> pin_list()
#> [1] "x" "y" "z"
```
