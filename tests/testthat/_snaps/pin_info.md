# can get info with or without board

    Code
      b <- board_temp()
      pin(mtcars[1:2], "mtcars2", board = b)
      pin_info("mtcars2", b)
    Output
      # Source: temp<mtcars2> [table]
      # Properties:
      #   version: 0
      #   created: .na.real
      #   file_size: 0
      #   meta:
      #   - type: table
      #     description: ~
      #     rows: 32
      #     cols: 2
      #     columns:
      #       mpg: numeric
      #       cyl: numeric
      #     path: mtcars2
      #     name: mtcars2
      #     api_version: 0

---

    Code
      pin(mtcars[1:2], "mtcars3", board = "test")
      pin_info("mtcars3", board = "test")
    Output
      # Source: test<mtcars3> [table]
      # Properties:
      #   version: 0
      #   created: .na.real
      #   file_size: 0
      #   meta:
      #   - type: table
      #     description: ~
      #     rows: 32
      #     cols: 2
      #     columns:
      #       mpg: numeric
      #       cyl: numeric
      #     path: mtcars3
      #     name: mtcars3
      #     api_version: 0

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

