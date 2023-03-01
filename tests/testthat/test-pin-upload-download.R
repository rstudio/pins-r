test_that("can round-trip a file", {
  board <- board_temp()

  path1 <- withr::local_tempfile()
  writeLines("Hi!", path1)

  pin_upload(board, path1, "greeting")
  path2 <- pin_download(board, "greeting")
  expect_equal(readLines(path2), "Hi!")
})

test_that("pin_upload generated useful messages", {
  local_mocked_bindings(version_name = function(metadata) "20120304T050607Z-xxxxx")
  ui_loud()
  board <- board_temp()

  expect_snapshot(error = TRUE, {
    pin_upload(board, 1:10)
    pin_upload(board, "this-path-doesn't-exist")

    path <- fs::file_touch(fs::path_temp("test.txt"))
    pin_upload(board, path)
  })
})

test_that("can pin a directory", {
  path <- withr::local_tempdir()
  writeLines("Hi!", fs::path(path, "a.txt"))
  writeLines("Hello :)", fs::path(path, "b.txt"))
  subpath <- fs::dir_create(fs::path(path, "subdir"))
  writeLines("Heyya", fs::path(subpath, "c.txt"))

  board <- board_temp()
  pin_upload(board, path, "test")
  # Currently squashes subdirs into top-level
  expect_equal(pin_meta(board, "test")$file, c("a.txt", "b.txt", "c.txt"))
  expect_equal(readLines(pin_download(board, "test")[[1]]), "Hi!")
  expect_equal(readLines(pin_download(board, "test")[[3]]), "Heyya")
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

test_that("informative error for legacy boards", {
  expect_snapshot(error = TRUE, {
    board <- legacy_temp()
    board %>% pin_upload(1:10, "x")
    board %>% pin_download("x")
  })
})
