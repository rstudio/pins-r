test_that("can read and write simple pin", {
  board <- board_s3_test()

  pin_write(board, data.frame(x = 1:3), "test-basic")
  expect_equal(pin_list(board), "test-basic")
  expect_equal(pin_read(board, "test-basic"), data.frame(x = 1:3))

  pin_delete(board, "test-basic")
  expect_equal(pin_list(board), character())
})

test_that("absent pins handled consistently", {
  board <- board_s3_test()
  pin_write(board, 1, "test-present")
  withr::defer(pin_delete(board, "test-present"))

  expect_equal(pin_list(board), "test-present")
  expect_equal(pin_exists(board, "test-present"), TRUE)
  expect_equal(pin_exists(board, "y"), FALSE)

  expect_error(pin_meta(board, "y"), class = "pins_pin_absent")
})

test_that("tracks versions as expected", {
  board <- board_s3_test()

  pin_write(board, 1, "test-version")
  withr::defer(pin_delete(board, "test-version"))

  versions <- pin_versions(board, "test-version")
  expect_equal(nrow(versions), 1)
  pin_write(board, 2, "test-version")
  pin_write(board, 3, "test-version")
  expect_equal(nrow(pin_versions(board, "test-version")), 3)

  x <- pin_read(board, "test-version", version = versions$version[[1]])
  expect_equal(x, 1)
})

test_that("if versioning off, overwrites existing version", {
  board <- board_s3_test(versioned = FALSE)

  pin_write(board, 1, "test-unversioned")
  withr::defer(pin_delete(board, "test-unversioned"))
  expect_equal(nrow(pin_versions(board, "test-unversioned")), 1)

  pin_write(board, 2, "test-unversioned")
  expect_equal(nrow(pin_versions(board, "test-unversioned")), 1)

  expect_equal(pin_read(board, "test-unversioned"), 2)
})

test_that("generates useful errors for missing pins/versions", {
  board <- board_s3_test()

  pin_write(board, 1, "test-error")
  pin_write(board, 2, "test-error")
  withr::defer(pin_delete(board, "test-error"))

  expect_snapshot(error = TRUE, {
    board %>% pin_versions("missing")

    board %>% pin_read("missing")
    board %>% pin_read("test-error", version = 1)
    board %>% pin_read("test-error", version = "missing")

    board %>% pin_write(3, "test-error", versioned = FALSE)
  })

})
