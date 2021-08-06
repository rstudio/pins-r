# generates useful errors for missing pins/versions

    Code
      board %>% pin_versions("missing")
    Error <pins_pin_absent>
      Can't find pin called 'missing'
      i Use `pin_list()` to see all available pins in this board
    Code
      board %>% pin_read("missing")
    Error <pins_pin_absent>
      Can't find pin called 'missing'
      i Use `pin_list()` to see all available pins in this board
    Code
      board %>% pin_read(name, version = 1)
    Error <rlang_error>
      `version` must be a string or `NULL`
    Code
      board %>% pin_read(name, version = "missing")
    Error <http_404>
      Not Found (HTTP 404). Failed to complete Storage Services operation. Message:
      .
    Code
      board %>% pin_write(3, name, versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested a write without versions
      i To un-version a pin, you must delete it

