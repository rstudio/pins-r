context("board local")

text_file <- dir(getwd(), recursive = TRUE, pattern = "hello.txt", full.names = TRUE)

test_that("local board is registered", {
  board_register("local", cache = tempfile())
  expect_true("local" %in% board_list())
})

test_that(paste("can pin() file with auto-generated name in local board"), {
  cached_path <- pin(text_file, board = "local")

  expect_true(is.character(cached_path))

  expect_equal(readLines(cached_path), "hello world")
})

board_test("local")
