# Delete a pin

Delete a pin (or pins), removing it from the board

## Usage

``` r
pin_delete(board, names, ...)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  or another `board_` function.

- names:

  The names of one or more pins to delete

- ...:

  Additional arguments passed on to methods for a specific board.

## Examples

``` r
board <- board_temp()
board |> pin_write(1:5, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20251110T190122Z-796ae'
#> Writing to pin 'x'
board |> pin_write(mtcars)
#> Using `name = 'mtcars'`
#> Guessing `type = 'rds'`
#> Creating new version '20251110T190122Z-e4808'
#> Writing to pin 'mtcars'
board |> pin_write(runif(1e6), "y")
#> Guessing `type = 'rds'`
#> Creating new version '20251110T190123Z-58351'
#> Writing to pin 'y'
board |> pin_list()
#> [1] "mtcars" "x"      "y"     

board |> pin_delete(c("x", "y"))
board |> pin_list()
#> [1] "mtcars"
```
