test_that("can read and write simple pin", {
  board <- board_s3_test()

  pin_write(board, data.frame(x = 1:3), "test-basic")
  expect_equal(pin_list(board), "test-basic")

  expect_equal(pin_read(board, "test-basic"), data.frame(x = 1:3))
  pin_delete(board, "test-basic")
  expect_equal(pin_list(board), character())
})

test_that("if versioning off, overwrites existing version", {
  board <- board_s3_test(versions = FALSE)

  pin_write(board, 1, "test-unversioned")
  withr::defer(pin_delete(board, "test-unversioned"))
  expect_equal(nrow(pin_versions(board, "test-unversioned")), 1)

  pin_write(board, 2, "test-unversioned")
  expect_equal(nrow(pin_versions(board, "test-unversioned")), 1)

  expect_equal(pin_read(board, "test-unversioned"), 2)
})

