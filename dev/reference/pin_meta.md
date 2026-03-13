# Retrieve metadata for a pin

Pin metadata comes from three sources:

- Standard metadata added by
  [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)/[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md).
  This includes:

  - `$name` - the pin's name.

  - `$file` - names of files stored in the pin.

  - `$file_size` - size of each file.

  - `$pin_hash` - hash of pin contents.

  - `$type` - type of pin: "rds", "csv", etc

  - `$title` - pin title

  - `$description` - pin description

  - `$tags` - pin tags

  - `$urls` - URLs for more info on pin

  - `$created` - date this (version of the pin) was created

  - `$api_version` - API version used by pin

- Metadata supplied by the user, stored in `$user`. This is untouched
  from what is supplied in
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)/[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  except for being converted to and from to YAML.

- Local metadata generated when caching the pin, stored in `$local`.
  This includes information like the version of the pin, and the path
  its local cache.

## Usage

``` r
pin_meta(board, name, version = NULL, ...)
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

- ...:

  Additional arguments passed on to methods for a specific board.

## Value

A list.

## Examples

``` r
b <- board_temp()
b |> pin_write(head(mtcars), "mtcars", metadata = list("Hadley" = TRUE))
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165847Z-d3684'
#> Writing to pin 'mtcars'

# Get the pin
b |> pin_read("mtcars")
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
# Get its metadata
b |> pin_meta("mtcars")
#> List of 13
#>  $ file       : chr "mtcars.rds"
#>  $ file_size  : 'fs_bytes' int 425
#>  $ pin_hash   : chr "d368458c1e1ab524"
#>  $ type       : chr "rds"
#>  $ title      : chr "mtcars: a pinned 6 x 11 data frame"
#>  $ description: NULL
#>  $ tags       : NULL
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-03-13 16:58:47"
#>  $ api_version: int 1
#>  $ user       :List of 1
#>   ..$ Hadley: logi TRUE
#>  $ name       : chr "mtcars"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/RtmpxKttBg/pins-1e481a50893f/mtcars/20260313T165847Z-d3684"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260313T165847Z-d3684"
# Get path to underlying data
b |> pin_download("mtcars")
#> [1] "/tmp/RtmpxKttBg/pins-1e481a50893f/mtcars/20260313T165847Z-d3684/mtcars.rds"

# Use tags instead
b |> pin_write(tail(mtcars), "mtcars", tags = c("fuel-efficiency", "automotive"))
#> Guessing `type = 'rds'`
#> Replacing version '20260313T165847Z-d3684' with
#> '20260313T165847Z-21aa2'
#> Writing to pin 'mtcars'
b |> pin_meta("mtcars")
#> List of 13
#>  $ file       : chr "mtcars.rds"
#>  $ file_size  : 'fs_bytes' int 465
#>  $ pin_hash   : chr "21aa2b764c9e4060"
#>  $ type       : chr "rds"
#>  $ title      : chr "mtcars: a pinned 6 x 11 data frame"
#>  $ description: NULL
#>  $ tags       : chr [1:2] "fuel-efficiency" "automotive"
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-03-13 16:58:47"
#>  $ api_version: int 1
#>  $ user       : list()
#>  $ name       : chr "mtcars"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/RtmpxKttBg/pins-1e481a50893f/mtcars/20260313T165847Z-21aa2"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260313T165847Z-21aa2"
```
