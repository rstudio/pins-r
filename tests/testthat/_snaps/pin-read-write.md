# can't pin_read() file that was pin_uploaded()

    Code
      pin_read(board, "test")
    Condition
      Error in `object_read()`:
      ! Cannot automatically read pin:
      * Is your pin specified as a full path? Retrieve it with `pin_download()`
      * Is your pin specified via a URL that is not a full path, such as a Posit Connect vanity URL? Remember to include a trailing slash `/`

# useful errors on bad inputs

    Code
      pin_write(mtcars)
    Condition
      Error in `pin_write()`:
      ! `board` must be a pin board, not a <data.frame> object.
    Code
      pin_write(board, mtcars, name = 1:10)
    Condition
      Error in `pin_write()`:
      ! `name` must be a string
    Code
      pin_write(board, mtcars, name = "mtcars", "json")
    Condition
      Error in `pin_write()`:
      ! Arguments after the dots `...` must be named, like `type = "json"`.
    Code
      pin_write(board, mtcars, name = "mtcars", type = "froopy-loops")
    Condition
      Error in `purrr::map2()`:
      i In index: 1.
      Caused by error in `map2_()`:
      ! `type` must be one of "rds", "json", "parquet", "arrow", "pickle", "csv", "qs", or "qs2", not "froopy-loops".
    Code
      pin_write(board, mtcars, name = "mtcars", metadata = 1)
    Condition
      Error in `pin_write()`:
      ! `metadata` must be a list or `NULL`, not the number 1.
    Code
      pin_write(board, mtcars, title = "title", tags = list(a = "a"))
    Condition
      Error in `pin_write()`:
      ! `tags` must be a character vector or `NULL`, not a list.
    Code
      pin_write(board, mtcars, title = "title", urls = list(a = "a"))
    Condition
      Error in `pin_write()`:
      ! `urls` must be a character vector or `NULL`, not a list.
    Code
      pin_write(board, mtcars, title = "title", metadata = c("tag1", "tag2"))
    Condition
      Error in `pin_write()`:
      ! `metadata` must be a list or `NULL`, not a character vector.

# pin_write() noisily generates name and type

    Code
      b <- board_temp()
      pin_write(b, mtcars)
    Message
      Using `name = 'mtcars'`
      Guessing `type = 'rds'`
      Creating new version '20120304T050607Z-xxxxx'
      Writing to pin 'mtcars'
    Code
      pin_write(b, data.frame(x = 1))
    Condition
      Error in `pin_write()`:
      ! Must supply `name` when `x` is an expression

# can request specific hash

    Code
      b <- board_temp()
      pin_write(b, mtcars, name = "mtcars", type = "rds")
    Message
      Creating new version '20120304T050607Z-xxxxx'
      Writing to pin 'mtcars'
    Code
      pin_read(b, "mtcars", hash = "ABCD")
    Condition
      Error in `pin_read()`:
      ! Specified hash "ABCD" doesn't match pin hash "dfa6c1c109362781".

# can write and read multiple types

    Code
      pin_read(board, "df-1", type = "froopy-loops")
    Condition
      Error in `pin_read()`:
      ! `type` must be one of "rds", "json", "parquet", "arrow", "pickle", "csv", "qs", "qs2", or "file", not "froopy-loops".
    Code
      pin_read(board, "df-1")
    Condition
      Warning:
      ! Pin "df-1" has multiple types: "rds" and "csv"
      * Automatically choosing "rds"
      * To avoid this warning, specify the `type` explicitly
    Output
      # A tibble: 10 x 1
             x
         <int>
       1     1
       2     2
       3     3
       4     4
       5     5
       6     6
       7     7
       8     8
       9     9
      10    10

