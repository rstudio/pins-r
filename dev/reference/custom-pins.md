# Custom Pins

Family of functions meant to be used to implement custom pin extensions,
not to be used by users.

## Usage

``` r
pin_load(path, ...)

board_pin_store(
  board,
  path,
  name,
  pin_metadata,
  extract = TRUE,
  retrieve = TRUE,
  zip = FALSE,
  cache = TRUE,
  metadata = NULL,
  custom_metadata = NULL,
  ...
)
```

## Arguments

- path:

  The path to store.

- ...:

  Additional parameteres.

- board:

  The board to extended, retrieved with
  [`board_get()`](https://pins.rstudio.com/dev/reference/board_register.md).

- name:

  The name of the pin.

- pin_metadata:

  A list of pin metadata describing the pin. Must contain `type` and
  `description`.

- retrieve:

  Should the pin be retrieved after being created? Defaults to `TRUE`.

- metadata:

  Additional user supplied metadata.

- custom_metadata:

  Deprecated. Please use `metadata` instead.
