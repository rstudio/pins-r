context("pin")

test_that("can pin() file", {
  file_path <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)

  cached_path <- pin(file_path, "afile")

  testthat::expect_true(is.character(cached_path))

  testthat::expect_equal(readLines(cached_path), "hello world")
})

test_that("can pin() file with auto-generated name", {
  file_path <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)

  cached_path <- pin(file_path)

  testthat::expect_true(is.character(cached_path))

  testthat::expect_equal(readLines(cached_path), "hello world")
})
