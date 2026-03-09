# Write a manifest YAML file for a board

This is a low-level function that powers
[`write_board_manifest()`](https://pins.rstudio.com/reference/write_board_manifest.md).
It is needed primarily for folks developing new board types, and should
not generally be called directly.

## Usage

``` r
write_board_manifest_yaml(board, manifest, ...)
```

## Arguments

- board:

  A pin board that is *not* read-only.

- manifest:

  Contents to be written to the manifest file, as a list.

- ...:

  Additional arguments passed on to methods for a specific board.

## Value

`write_board_manifest_yaml()` is called for its side-effect of writing a
manifest YAML file.
