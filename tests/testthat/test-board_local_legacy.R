test_that("local board registered by default", {
  expect_true("local" %in% board_list())
})

test_that(paste("can pin() file with auto-generated name in local board"), {
  b <- board_temp()

  expect_equal(pin_list(b), character())

  hello <- test_path("files/hello.txt")
  path <- pin(hello, board = b)
  expect_true(file.exists(path))
  expect_equal(readLines(path), "hello world")

  expect_equal(pin_list(b), "hello")
})

test_that("can version a local pin", {
  b <- board_temp(versions = TRUE)

  versions <- pin_versions("df", board = b)
  expect_equal(
    versions,
    tibble::tibble(version = character(), created = .POSIXct(integer()))
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

