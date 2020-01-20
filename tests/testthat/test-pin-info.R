context("pin info")

test_that("can retrieve pin_info() across all boards", {
  pin(iris, "iris")

  entries <- pin_info("iris")

  expect_equal(entries$name, "iris")
  expect_equal(entries$type, "table")
  expect_equal(as.character(entries$rows), "150")
})

test_that("can retrieve pin_info() with no extended info across all boards", {
  pin(iris, "iris")

  entries <- pin_info("iris", extended = FALSE)

  expect_equal(entries$name, "iris")
  expect_equal(entries$type, "table")
  expect_equal(as.character(entries$rows), "150")
})

test_that("can retrieve pin_info() with no metadata info across all boards", {
  pin(iris, "iris")

  entries <- pin_info("iris", metadata = FALSE)

  expect_equal(entries$name, "iris")
  expect_equal(entries$type, "table")
  expect_equal("rows" %in% colnames(entries$rows), FALSE)
})

test_that("can retrieve pin_info() with no extended nor metadata info across all boards", {
  pin(iris, "iris")

  entries <- pin_info("iris", extended = FALSE, metadata = FALSE)

  expect_equal(entries$name, "iris")
  expect_equal(entries$type, "table")
  expect_equal("rows" %in% colnames(entries$rows), FALSE)
})
