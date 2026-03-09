# Use a local folder as board

- `board_folder()` creates a board inside a folder. You can use this to
  share files by using a folder on a shared network drive or inside a
  DropBox.

- `board_local()` creates a board in a system data directory. It's
  useful if you want to share pins between R sessions on your computer,
  and you don't care where the data lives.

- `board_temp()` creates a temporary board that lives in a session
  specific temporary directory. It will be automatically deleted once
  the current R session ends. It's useful for examples and tests.

## Usage

``` r
board_folder(path, versioned = FALSE)

board_local(versioned = FALSE)

board_temp(versioned = FALSE)
```

## Arguments

- path:

  Path to directory to store pins. Will be created if it doesn't already
  exist.

- versioned:

  Should this board be registered with support for versions?

## See also

Other boards:
[`board_connect()`](https://pins.rstudio.com/reference/board_connect.md),
[`board_connect_url()`](https://pins.rstudio.com/reference/board_connect_url.md),
[`board_url()`](https://pins.rstudio.com/reference/board_url.md)

## Examples

``` r
# session-specific local board
board <- board_temp()
```
