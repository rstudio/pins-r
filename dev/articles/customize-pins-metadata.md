# Create consistent metadata for pins

The `metadata` argument in pins is flexible and can hold any kind of
metadata that you can formulate as a
[`list()`](https://rdrr.io/r/base/list.html). In some situations, you
may want to read and write with *consistent* customized metadata; you
can create functions to wrap
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) for
your particular use case.

To see a different approach for when you want to write and read whole
file(s) in a customized way, see
[`vignette("managing-custom-formats")`](https://pins.rstudio.com/dev/articles/managing-custom-formats.md).

We’ll begin by creating a temporary board for demonstration:

``` r

library(pins)

board <- board_temp()
```

## A function to store factors

Say you want to store a factor as JSON together with the *levels* of the
factor in the metadata. We can write a function wrapping
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) that
creates the standardized metadata we are interested in and writes it in
a consistent way.

``` r

pin_write_factor_json <- function(board, 
                                  x, 
                                  name, 
                                  title = NULL, 
                                  description = NULL, 
                                  metadata = list(), 
                                  versioned = NULL, 
                                  tags = NULL, 
                                  ...) {
  if (!is.factor(x)) rlang::abort("`x` is not a factor")
  factor_levels <- levels(x)
  metadata <- modifyList(metadata, list(factor_levels = factor_levels))
  pin_write(
    board = board, 
    x = x, 
    name = name, 
    type = "json", 
    title = title, 
    description = description, 
    metadata = metadata,
    ...
  )
}
```

We can use this new function to write a pin as JSON with our specific
metadata:

``` r

ten_letters <- factor(sample(letters, size = 10), levels = letters)
board |> pin_write_factor_json(ten_letters, "letters-as-json")
#> Creating new version '20260802T194609Z-099e2'
#> Writing to pin 'letters-as-json'
```

### A function to read factors

It’s possible to read this pin using the regular
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md)
function, but the object we get is no longer a factor!

``` r

board |> pin_read("letters-as-json")
#>  [1] "m" "w" "l" "e" "f" "o" "u" "d" "v" "i"
```

Instead, we can also write a special function for reading, to
reconstruct the factor including its levels:

``` r

pin_read_factor_json <- function(board, name, version = NULL, hash = NULL, ...) {
  ret <- pin_read(board = board, name = name, version = version, hash = hash, ...)
  meta <- pin_meta(board = board, name = name, version = version, ...)
  factor(ret, levels = meta$user$factor_levels)
}

board |> pin_read_factor_json("letters-as-json")
#>  [1] m w l e f o u d v i
#> Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
```

### Examples of using consistent metadata

How are these approaches used in real projects?

- The vetiver package wraps pins functions [to write and read model
  binaries together with their metadata, including an renv
  lockfile](https://github.com/rstudio/vetiver-r/blob/main/R/pin-read-write.R).
- You can record version control information such as Git commit and SHA
  as pin metadata.
- You can create data lineage or data governance metadata appropriate to
  your use case.
