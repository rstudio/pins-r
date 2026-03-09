# Search for pins (legacy API)

**\[deprecated\]**

## Usage

``` r
pin_find(
  text = NULL,
  board = NULL,
  name = NULL,
  extended = FALSE,
  metadata = FALSE,
  ...
)
```

## Arguments

- text:

  The text to find in the pin description or name.

- board:

  The board name used to find the pin.

- name:

  The exact name of the pin to match when searching.

- extended:

  Should additional board-specific columns be shown?

- metadata:

  Include pin metadata in results?

- ...:

  Additional parameters.

## Details

Search for pins in legacy boards.
