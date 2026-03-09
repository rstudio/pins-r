# Fetch/store a pin

These are low-level functions that power
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md),
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md),
[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md),
and
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md).
They are needed primarily for folks developing new board types, and
should not generally be called directly.

## Usage

``` r
pin_fetch(board, name, version = NULL, ...)

pin_store(board, name, paths, metadata, versioned = NULL, x = NULL, ...)
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

- paths:

  A character vector of file paths to upload to `board`.

- metadata:

  A list containing additional metadata to store with the pin. When
  retrieving the pin, this will be stored in the `user` key, to avoid
  potential clashes with the metadata that pins itself uses.

- versioned:

  Should the pin be versioned? The default, `NULL`, will use the default
  for `board`

- x:

  An object (typically a data frame) to pin.

## Value

`pin_fetch()` is called primarily for its side-effect of downloading
remote pins into the local cache. It returns the same data as
[pin_meta](https://pins.rstudio.com/dev/reference/pin_meta.md).
`pin_store()` is called for its side-effect of uploading a local file to
a remote board. It invisibly returns the fully qualified pin name.

## Examples

``` r
board <- board_temp()

board |> pin_upload(system.file("CITATION"))
#> Guessing `name = 'CITATION'`
#> Creating new version '20260309T150955Z-f5b9d'
path <- board |> pin_download("CITATION")
path
#> [1] "/tmp/RtmpAsTszR/pins-1af619b8fce2/CITATION/20260309T150955Z-f5b9d/CITATION"
readLines(path)[1:5]
#> [1] "bibentry(\"Manual\","                                                         
#> [2] "         title = \"R: A Language and Environment for Statistical Computing\","
#> [3] "         author = person(\"R Core Team\"),"                                   
#> [4] "         organization = \"R Foundation for Statistical Computing\","          
#> [5] "         address      = \"Vienna, Austria\","                                 
```
