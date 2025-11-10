# Retrieve pin metadata (legacy API)

**\[deprecated\]**

## Usage

``` r
pin_info(
  name,
  board = NULL,
  extended = TRUE,
  metadata = TRUE,
  signature = FALSE,
  ...
)
```

## Arguments

- name:

  The exact name of the pin to match when searching.

- board:

  The board name used to find the pin.

- extended:

  Should additional board-specific information be shown?

- metadata:

  Should additional pin-specific information be shown?

- signature:

  Should a signature to identify this pin be shown?

- ...:

  Additional parameters.

## Details

Retrieve metadata for pins in legacy boards.
