# Custom Boards

**\[deprecated\]**

## Usage

``` r
board_pin_create(board, path, name, metadata, ...)

board_initialize(board, ...)

board_browse(board, ...)

board_desc(board, ...)

board_pin_get(board, name, ...)

board_pin_remove(board, name, ...)

board_pin_find(board, text, ...)

board_pin_versions(board, name, ...)
```

## Arguments

- board:

  The board to extend, retrieved with
  [`board_get()`](https://pins.rstudio.com/dev/reference/board_register.md).

- path:

  The path to store as a pin.

- name:

  The name of the pin.

- metadata:

  A list of metadata associated with this pin.

- ...:

  Additional parameters.

- text:

  The text pattern to find a pin.

## Details

Family of functions meant to be used to implement custom boards
extensions, not to be used by users.
