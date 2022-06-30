# can get info with or without board

    Code
      b <- legacy_temp()
      pin(mtcars[1:2], "mtcars2", board = b)
      pin_info("mtcars2", b)
    Output
      # Source: temp<mtcars2> [table]
      # Properties:
      #   rows: 32
      #   cols: 2
      #   columns:
      #     mpg: numeric
      #     cyl: numeric

---

    Code
      pin(mtcars[1:2], "mtcars3", board = "test")
      pin_info("mtcars3", board = "test")
    Output
      # Source: test<mtcars3> [table]
      # Properties:
      #   rows: 32
      #   cols: 2
      #   columns:
      #     mpg: numeric
      #     cyl: numeric

# gives useful errors

    Code
      pin_info("mtcars2")
    Condition
      Error in `pin_info()`:
      ! Pin 'mtcars2' was not found.
    Code
      pin(mtcars[1:2], "mtcars2", board = "test1")
      pin(mtcars[1:2], "mtcars2", board = "test2")
      pin_info("mtcars2")
    Condition
      Error in `pin_info()`:
      ! Pin 'mtcars2' was found in multiple boards: test1,test2

# gives informative error if used with modern board

    Code
      board <- board_temp()
      pin_info("mtcars", board = board)
    Condition
      Error in `this_not_that()`:
      ! Use `pin_meta()` with this board, not `pin_info()`

