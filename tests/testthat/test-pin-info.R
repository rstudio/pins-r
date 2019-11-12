context("pin info")

test_that("can retrieve pin_info() across all boards", {
  pin(iris, "iris")

  entries <- pin_info("iris")

  expect_equal(entries$name, "iris")
  expect_equal(entries$type, "table")
  expect_equal(as.character(entries$rows), "150")
})
