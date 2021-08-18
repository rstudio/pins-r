test_api_basic(board_temp())
test_api_versioning(board_temp(versioned = TRUE))

test_that("has useful print method", {
  expect_snapshot(board_folder("/tmp/test", name = "test"))
})

test_that("can upload/download multiple files", {
  path1 <- withr::local_tempfile()
  writeLines("a", path1)
  path2 <- withr::local_tempfile()
  writeLines("b", path2)

  board <- board_temp()
  suppressMessages(pin_upload(board, c(path1, path2), "test"))

  out <- pin_download(board, "test")
  expect_equal(length(out), 2)
  expect_equal(readLines(out[[1]]), "a")
  expect_equal(readLines(out[[2]]), "b")
})

test_that("can browse", {
  b <- board_folder("/tmp/test", name = "test")

  b %>% pin_write(1:10, "x")
  withr::defer(b %>% pin_delete("x"))

  expect_snapshot(b %>% pin_browse("x"))
  expect_snapshot(b %>% pin_browse("x", cache = TRUE), error = TRUE)
})

test_that("generates useful messages", {
  ui_loud()
  b <- board_temp()
  expect_snapshot(error = TRUE, {
    pin_read(b, "x")
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:6, "x", type = "rds")
  })
})
