# Determine required packages for a pins board

Determine required packages for a pins board

## Usage

``` r
# S3 method for class 'pins_board_azure'
required_pkgs(x, ...)

# S3 method for class 'pins_board_connect'
required_pkgs(x, ...)

# S3 method for class 'pins_board_databricks'
required_pkgs(x, ...)

# S3 method for class 'pins_board_gcs'
required_pkgs(x, ...)

# S3 method for class 'pins_board_gdrive'
required_pkgs(x, ...)

# S3 method for class 'pins_board_ms365'
required_pkgs(x, ...)

# S3 method for class 'pins_board_s3'
required_pkgs(x, ...)

# S3 method for class 'pins_board'
required_pkgs(x, ...)
```

## Arguments

- x:

  A pin board

- ...:

  Not used.

## Value

A character vector of package names required to use the board.

## Examples

``` r
required_pkgs(board_temp())
#> character(0)
```
