context("pin")

text_file <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)

test_that("can pin() file", {
  cached_path <- pin(text_file, "afile")

  expect_true(is.character(cached_path))

  expect_equal(readLines(cached_path), "hello world")
})

test_that("can pin() file with auto-generated name", {
  cached_path <- pin(text_file)

  expect_true(is.character(cached_path))

  expect_equal(readLines(cached_path), "hello world")
})

test_that("can pin_get() a pin", {
  pin(text_file, "afile")

  cached_path <- pin_get("afile")

  expect_true(is.character(cached_path))

  expect_equal(readLines(cached_path), "hello world")
})

test_that("can pin_find() a in any board", {
  pin(text_file, "afile")

  results <- pin_find("afile")

  expect_true("afile" %in% results$name)
})

test_that("can pin_find() in local board", {
  pin(text_file, "afile")

  results <- pin_find("afile", board = "local")

  expect_true("afile" %in% results$name)
})
