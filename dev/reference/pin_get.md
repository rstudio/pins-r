# Retrieve a pin (legacy API)

**\[deprecated\]**

## Usage

``` r
pin_get(
  name,
  board = NULL,
  cache = TRUE,
  extract = NULL,
  version = NULL,
  files = FALSE,
  signature = NULL,
  ...
)
```

## Arguments

- name:

  The name of the pin.

- board:

  The board where this pin will be retrieved from.

- cache:

  Should the pin cache be used? Defaults to `TRUE`.

- extract:

  Should compressed files be extracted? Each board defines the default
  behavior.

- version:

  The version of the dataset to retrieve, defaults to latest one.

- files:

  Should only the file names be returned?

- signature:

  Optional signature to validate this pin, use
  [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md) to
  compute signature.

- ...:

  Additional parameters.

## Details

Retrieves a pin by name from the local or given board.

`pin_get()` retrieves a pin by name and, by default, from the local
board. You can use the `board` parameter to specify which board to
retrieve a pin from. If a board is not specified, it will use
[`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) to
find the pin across all boards and retrieve the one that matches by
name.
