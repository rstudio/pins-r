# Deparse a board object

Returns the R code that would recreate the board when re-run on another
computer. Goal is to capture the location of the board, but not the
authorisation, since (a) that would leak credentials and (b) in most
deployment scenarios board auth will be read from env vars.

## Usage

``` r
board_deparse(board, ...)

# S3 method for class 'pins_board_azure'
board_deparse(board, ...)

# S3 method for class 'pins_board_connect'
board_deparse(board, ...)

# S3 method for class 'pins_board_folder'
board_deparse(board, ...)

# S3 method for class 'pins_board_gcs'
board_deparse(board, ...)

# S3 method for class 'pins_board_kaggle_competition'
board_deparse(board, ...)

# S3 method for class 'pins_board_kaggle_dataset'
board_deparse(board, ...)

# S3 method for class 'pins_board_s3'
board_deparse(board, ...)

# S3 method for class 'pins_board_url'
board_deparse(board, ...)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  or another `board_` function.

- ...:

  Additional arguments passed on to methods for a specific board.

## Value

A call.

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_connect()
# Generate code to access this board from elsewhere
board_deparse(board)
} # }
```
