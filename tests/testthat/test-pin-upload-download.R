test_that("can round-trip a file", {
  board <- board_temp()

  path1 <- withr::local_tempfile()
  writeLines("Hi!", path1)

  pin_upload(board, path1, "greeting")
  path2 <- pin_download(board, "greeting")
  expect_equal(readLines(path2), "Hi!")
})

test_that("pin_upload generated useful messages", {
  ui_loud()
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin_upload(board, 1:10)
    pin_upload(board, "this-path-doesn't-exist")

    path <- fs::file_touch(fs::path_temp("test.txt"))
    pin_upload(board, path)
  })
})

test_that("can pin file called data.txt", {
  path <- withr::local_tempdir()
  writeLines("Hi!", fs::path(path, "data.txt"))

  board <- board_temp()
  expect_snapshot(pin_upload(board, fs::path(path, "data.txt")), error = TRUE)
})

test_that("user can supply metadata", {
  path1 <- withr::local_tempfile()
  writeLines("Hi!", path1)

  board <- board_temp()
  pin_upload(board, path1, "x", metadata = list(name = "Susan"), desc = "A vector")
  meta <- pin_meta(board, "x")
  expect_equal(meta$user, list(name = "Susan"))
  expect_equal(meta$description, "A vector")
})
