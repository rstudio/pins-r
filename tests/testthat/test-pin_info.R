test_that("can get info with or without board", {
  expect_snapshot({
    b <- board_local(tempfile())
    pin_write(b, mtcars[1:2], "mtcars2")
    pin_info("mtcars2", b)
  })

  local_register(board_local(tempfile(), name = "test"))
  expect_snapshot({
    pin(mtcars[1:2], "mtcars2", board = "test")
    pin_info("mtcars2")
  })
})

test_that("gives useful errors", {
  local_register(board_local(tempfile(), name = "test1"))
  local_register(board_local(tempfile(), name = "test2"))

  expect_snapshot(error = TRUE, {
    pin_info("mtcars2")

    pin(mtcars[1:2], "mtcars2", board = "test1")
    pin(mtcars[1:2], "mtcars2", board = "test2")
    pin_info("mtcars2")
  })
})
