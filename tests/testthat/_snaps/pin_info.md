# can get info with or without board

    Code
      b <- board_temp()
      pin_write(b, mtcars[1:2], "mtcars2")
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
      pin(mtcars[1:2], "mtcars2", board = "test")
      pin_info("mtcars2")
    Output
      # Source: test<mtcars2> [table]
      # Properties:
      #   rows: 32
      #   cols: 2
      #   columns:
      #     mpg: numeric
      #     cyl: numeric

# gives useful errors

    Code
      pin_info("mtcars2")
    Error <rlang_error>
      Pin 'mtcars2' was not found.
    Code
      pin(mtcars[1:2], "mtcars2", board = "test1")
      pin(mtcars[1:2], "mtcars2", board = "test2")
      pin_info("mtcars2")
    Error <rlang_error>
      Pin 'mtcars2' was found in multiple boards: test1,test2

