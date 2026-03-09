# Upload and download files to and from a board

This is a lower-level interface than
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) that
you can use to pin any file, as opposed to any R object. The path
returned by `pin_download()` is a read-only path to a cached file: you
should never attempt to modify this file.

## Usage

``` r
pin_download(board, name, version = NULL, hash = NULL, ...)

pin_upload(
  board,
  paths,
  name = NULL,
  ...,
  title = NULL,
  description = NULL,
  metadata = NULL,
  tags = NULL,
  urls = NULL
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

- ...:

  Additional arguments passed on to methods for a specific board.

- paths:

  A character vector of file paths to upload to `board`.

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

- tags:

  A character vector of tags for the pin; most important for
  discoverability on shared boards.

- urls:

  A character vector of URLs for more info on the pin, such as a link to
  a wiki or other documentation.

## Value

`pin_download()` returns a character vector of file paths;
`pin_upload()` returns the fully qualified name of the new pin,
invisibly.

## Examples

``` r
board <- board_temp()

board |> pin_upload(system.file("CITATION"))
#> Guessing `name = 'CITATION'`
#> Creating new version '20260309T150955Z-f5b9d'
path <- board |> pin_download("CITATION")
path
#> [1] "/tmp/RtmpAsTszR/pins-1af63bd7ef0a/CITATION/20260309T150955Z-f5b9d/CITATION"
readLines(path)[1:5]
#> [1] "bibentry(\"Manual\","                                                         
#> [2] "         title = \"R: A Language and Environment for Statistical Computing\","
#> [3] "         author = person(\"R Core Team\"),"                                   
#> [4] "         organization = \"R Foundation for Statistical Computing\","          
#> [5] "         address      = \"Vienna, Austria\","                                 
```
