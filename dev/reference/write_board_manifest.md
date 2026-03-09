# Write board manifest file to board's root directory

A board manifest file records all the pins, along with their versions,
stored on a board. This can be useful for a board built using, for
example,
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
or [`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md),
then served as a website, such that others can consume using
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md).
The manifest file is *not* versioned like a pin is, and this function
will overwrite any existing `_pins.yaml` file on your board. It is your
responsibility as the user to keep the manifest up to date.

Some examples are provided in
[`vignette("using-board-url")`](https://pins.rstudio.com/dev/articles/using-board-url.md).

## Usage

``` r
write_board_manifest(board, ...)
```

## Arguments

- board:

  A pin board that is *not* read-only.

- ...:

  Additional arguments passed on to methods for a specific board.

## Value

The board, invisibly

## Details

This function is not supported for read-only boards. It is called for
the side-effect of writing a manifest file, `_pins.yaml`, to the root
directory of the `board`. (This will not work in the unlikely event that
you attempt to create a pin called `"_pins.yaml"`.)

The behavior of the legacy API (for example,
[`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md)) is
unspecified once you have written a board manifest file to a board's
root directory. We recommend you only use `write_board_manifest()` with
modern boards.

## Examples

``` r
board <- board_temp()
pin_write(board, mtcars, "mtcars-csv", type = "csv")
#> Creating new version '20260309T151000Z-48c73'
#> Writing to pin 'mtcars-csv'
pin_write(board, mtcars, "mtcars-json", type = "json")
#> Creating new version '20260309T151000Z-c2702'
#> Writing to pin 'mtcars-json'

write_board_manifest(board)
#> Manifest file written to root folder of board, as `_pins.yaml`

# see the manifest's format:
fs::path(board$path, "_pins.yaml") |> readLines() |> cat(sep = "\n")
#> mtcars-csv:
#> - mtcars-csv/20260309T151000Z-48c73/
#> mtcars-json:
#> - mtcars-json/20260309T151000Z-c2702/

# if you write another pin, the manifest file is out of date:
pin_write(board, 1:10, "nice-numbers", type = "json")
#> Creating new version '20260309T151000Z-c3943'
#> Writing to pin 'nice-numbers'

# you decide when to update the manifest:
write_board_manifest(board)
#> Manifest file written to root folder of board, as `_pins.yaml`
```
