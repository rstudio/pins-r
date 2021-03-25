test_that("can round-trip a file", {
  withr::local_options(pins.quiet = TRUE)

  board <- board_temp()

  path1 <- withr::local_tempfile()
  writeLines("Hi!", path1)

  pin_upload(board, path1, "greeting")
  path2 <- pin_download(board, "greeting")
  expect_equal(readLines(path2), "Hi!")
})

test_that("pin_upload generated useful messages", {
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin_upload(board, 1:10)
    pin_upload(board, "this-path-doesn't-exist")

    path <- fs::file_touch(fs::path_temp("test.txt"))
    pin_upload(board, path)
  })
})
