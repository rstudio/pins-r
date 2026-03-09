# Local board (legacy API)

**\[deprecated\]**

`legacy_local()` powers `board_register_local()`, which allows you to
access local pins created in earlier versions of the pins package. For
new pins, we recommend that you transition to
[`board_local()`](https://pins.rstudio.com/reference/board_folder.md)
which supports the new pins API.

`legacy_temp()` creates a legacy board in a temporary location, for use
in tests and examples.

## Usage

``` r
legacy_local(path = NULL, name = "local", versions = FALSE)

board_register_local(name = "local", cache = NULL, ...)

legacy_temp(name = "temp", ...)
```

## Arguments

- path:

  Path where pins will be stored. If not supplied, defaults to a system
  **cache** directory, which may be deleted by the operating system if
  you run out of disk space.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- versions:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- ...:

  Additional parameters required to initialize a particular board.

## Examples

``` r
if (FALSE) { # \dontrun{
# Old api
pin(data.frame(x = 1:3), "test")
pin_get("test")

# New api
board <- board_local()
board |> pin_write(data.frame(x = 1:3), "test")
board |> pin_read("test")
} # }
```
