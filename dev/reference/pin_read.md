# Read and write objects to and from a board

Use `pin_write()` to pin an object to board, and `pin_read()` to
retrieve it.

## Usage

``` r
pin_read(board, name, version = NULL, hash = NULL, type = NULL, ...)

pin_write(
  board,
  x,
  name = NULL,
  ...,
  type = NULL,
  title = NULL,
  description = NULL,
  metadata = NULL,
  versioned = NULL,
  tags = NULL,
  urls = NULL,
  force_identical_write = FALSE
)
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

- hash:

  Specify a hash to verify that you get exactly the dataset that you
  expect. You can find the hash of an existing pin by looking for
  `pin_hash` in
  [`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md).

- type:

  File types used to save `x` to disk. Supports a single type or a
  vector of types (to pin in more than one format. Each type must be one
  of "csv", "json", "rds", "parquet", "arrow", or "qs2". If not
  supplied, will use JSON for bare lists and RDS for everything else. Be
  aware that CSV and JSON are plain text formats, while RDS, Parquet,
  Arrow, and [qs2](https://CRAN.R-project.org/package=qs2) are binary
  formats.

- ...:

  Additional arguments passed on to methods for a specific board.

- x:

  An object (typically a data frame) to pin.

- title:

  A title for the pin; most important for shared boards so that others
  can understand what the pin contains. If omitted, a brief description
  of the contents will be automatically generated.

- description:

  A detailed description of the pin contents.

- metadata:

  A list containing additional metadata to store with the pin. When
  retrieving the pin, this will be stored in the `user` key, to avoid
  potential clashes with the metadata that pins itself uses.

- versioned:

  Should the pin be versioned? The default, `NULL`, will use the default
  for `board`

- tags:

  A character vector of tags for the pin; most important for
  discoverability on shared boards.

- urls:

  A character vector of URLs for more info on the pin, such as a link to
  a wiki or other documentation.

- force_identical_write:

  Store the pin even if the pin contents are identical to the last
  version (compared using the hash). Only the pin contents are compared,
  not the pin metadata. Defaults to `FALSE`.

## Value

`pin_read()` returns an R object read from the pin; `pin_write()`
returns the fully qualified name of the new pin, invisibly.

## Details

`pin_write()` takes care of the details of serialising an R object to
disk, controlled by the `type` argument. See
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)/[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
if you want to perform the serialisation yourself and work just with
files.

## Examples

``` r
b <- board_temp(versioned = TRUE)

b |> pin_write(1:10, "x", description = "10 numbers")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T210908Z-8c3c9'
#> Writing to pin 'x'
b
#> Pin board <pins_board_folder>
#> Path: '/tmp/RtmpGqXXm7/pins-1d2a48749d11'
#> Cache size: 0

b |> pin_meta("x")
#> List of 13
#>  $ file       : chr "x.rds"
#>  $ file_size  : 'fs_bytes' int 61
#>  $ pin_hash   : chr "8c3c9a25ef4991ce"
#>  $ type       : chr "rds"
#>  $ title      : chr "x: a pinned integer vector"
#>  $ description: chr "10 numbers"
#>  $ tags       : NULL
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-03-13 21:09:08"
#>  $ api_version: int 1
#>  $ user       : list()
#>  $ name       : chr "x"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/RtmpGqXXm7/pins-1d2a48749d11/x/20260313T210908Z-8c3c9"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260313T210908Z-8c3c9"
b |> pin_read("x")
#>  [1]  1  2  3  4  5  6  7  8  9 10

# Add a new version
b |> pin_write(2:11, "x")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T210908Z-bcc36'
#> Writing to pin 'x'
b |> pin_read("x")
#>  [1]  2  3  4  5  6  7  8  9 10 11

# Retrieve an older version
b |> pin_versions("x")
#> # A tibble: 2 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260313T210908Z-8c3c9 2026-03-13 21:09:08 8c3c9
#> 2 20260313T210908Z-bcc36 2026-03-13 21:09:08 bcc36
b |> pin_read("x", version = .Last.value$version[[1]])
#>  [1]  2  3  4  5  6  7  8  9 10 11
# (Normally you'd specify the version with a string, but since the
# version includes the date-time I can't do that in an example)

 # Pin with multiple types
 b |> pin_write(1:10, "y", type = c("rds", "json"))
#> Creating new version '20260313T210908Z-a6b34'
#> Writing to pin 'y'
 b |> pin_read("y", type = "json")
#>  [1]  1  2  3  4  5  6  7  8  9 10
 # Automatically chooses one of the available types
 b |> pin_read("y")
#> Warning: ! Pin "y" has multiple types: "rds" and "json"
#> • Automatically choosing "rds"
#> • To avoid this warning, specify the `type` explicitly
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
