# Reactive Pin (legacy API)

**\[deprecated\]**

## Usage

``` r
pin_reactive(name, board, interval = 5000, session = NULL, extract = NULL)
```

## Arguments

- name:

  The name of the pin.

- board:

  The board where this pin will be retrieved from.

- interval:

  Approximate number of milliseconds to wait to retrieve updated pin.
  This can be a numeric value, or a function that returns a numeric
  value.

- session:

  The user session to associate this file reader with, or NULL if none.
  If non-null, the reader will automatically stop when the session ends.

- extract:

  Should compressed files be extracted? Each board defines the deefault
  behavior.

## Details

Creates a pin that reacts to changes in the given board by polling
[`pin_get()`](https://pins.rstudio.com/reference/pin_get.md), useful
when used from the `shiny` package.
