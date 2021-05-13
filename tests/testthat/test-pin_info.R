test_that("can get info with or without board", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_snapshot({
    b <- legacy_temp()
    pin(mtcars[1:2], "mtcars2", board = b)
    pin_info("mtcars2", b)
  })

  local_register(legacy_temp("test"))
  expect_snapshot({
    pin(mtcars[1:2], "mtcars3", board = "test")
    pin_info("mtcars3", board = "test")
  })
})

test_that("gives useful errors", {
  withr::local_options(lifecycle_verbosity = "quiet")
  local_register(legacy_temp("test1"))
  local_register(legacy_temp("test2"))

  expect_snapshot(error = TRUE, {
    pin_info("mtcars2")

    pin(mtcars[1:2], "mtcars2", board = "test1")
    pin(mtcars[1:2], "mtcars2", board = "test2")
    pin_info("mtcars2")
  })
})

test_that("gives informative error if used with modern board", {
  expect_snapshot(error = TRUE, {
    board <- board_temp()
    pin_info("mtcars", board = board)
  })
})
