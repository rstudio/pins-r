# generates useful errors for missing pins/versions

    Code
      board %>% pin_versions("missing")
    Error <rlang_error>
      Can't find pin called 'missing'
    Code
      board %>% pin_read("missing")
    Error <rlang_error>
      Can't find pin called 'missing'
    Code
      board %>% pin_read("test-error", version = 1)
    Error <rlang_error>
      `version` must be a string or `NULL`
    Code
      board %>% pin_read("test-error", version = "missing")
    Error <rlang_error>
      Can't find version 'missing' of 'test-error' pin
    Code
      board %>% pin_write(3, "test-error", versioned = FALSE)
    Error <rlang_error>
      Pin is versioned, but you have requested a write without versions
      i To un-version a pin, you must delete it

