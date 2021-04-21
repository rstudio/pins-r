test_that("local board registered by default", {
  expect_true("local" %in% board_list())
})

test_that("has useful print method", {
  expect_snapshot(board_folder("/tmp/test", name = "test"))
})

test_that(paste("can pin() file with auto-generated name in local board"), {
  b <- board_temp()

  expect_equal(nrow(pin_find(board = b)), 0)

  hello <- test_path("files/hello.txt")
  path <- pin(hello, board = b)
  expect_true(file.exists(path))
  expect_equal(readLines(path), "hello world")

  pins <- pin_find(, board = b)
  expect_equal(pins$name, "hello")
})

test_that("can remove a local pin", {
  b <- board_temp()

  pin(mtcars, board = b)
  expect_true(file.exists(pin_registry_path(b, "mtcars")))

  pin_remove("mtcars", board = b)
  expect_false(file.exists(pin_registry_path(b, "mtcars")))
})

test_that("can version a local pin", {
  b <- board_temp(versions = TRUE)

  versions <- pin_versions("df", board = b)
  expect_equal(
    versions,
    wibble(version = character(), created = .POSIXct(integer()))
  )

  pin(data.frame(x = 1), "df", board = b)
  pin(data.frame(x = 2), "df", board = b)
  pin(data.frame(x = 3), "df", board = b)

  versions <- pin_versions("df", board = b)
  expect_equal(nrow(versions), 3)

  newest <- pin_get("df", version = versions$version[[1]], board = b)
  oldest <- pin_get("df", version = versions$version[[3]], board = b)
  expect_equal(newest$x, 3)
  expect_equal(oldest$x, 1)
})


# pins 1.0.0 --------------------------------------------------------------

test_that("can get versions", {
  b <- board_temp()
  expect_snapshot(pin_write(b, 1:5, "x", type = "rds", versioned = TRUE))
  first_version <- pin_versions(b, "x")$version
  expect_equal(length(first_version), 1)

  expect_snapshot(pin_write(b, 1:6, "x", type = "rds", versioned = TRUE))
  expect_equal(length(pin_versions(b, "x")$version), 2)

  expect_equal(pin_read(b, "x"), 1:6)
  expect_equal(pin_read(b, "x", version = first_version), 1:5)
  expect_snapshot(pin_read(b, "x", version = "xxx"), error = TRUE)
})

test_that("can pin_read() pins made by pin()", {
  withr::local_options(pins.quiet = TRUE)
  board <- board_temp()

  # pin.data.frame
  df <- data.frame(x = 1:10)
  pin(df, "df-1", board = board)
  expect_equal(pin_read(board, "df-1"), df)

  # pin.character (files)

  # pin.default
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

test_that("can't unversion an unversioned pin", {
  expect_snapshot(error = TRUE, {
    b <- board_temp(versions = TRUE)
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:5, "x", type = "rds", versioned = FALSE)
  })
})

test_that("generates useful messages", {
  b <- board_temp()
  expect_snapshot(error = TRUE, {
    pin_read(b, "x")
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:5, "x", type = "rds")
    pin_write(b, 1:6, "x", type = "rds")
  })
})
