board_test(legacy_temp(), suite = "default")
board_test(legacy_temp(versions = TRUE), suite = "versions")

test_that("local board registered by default", {
  expect_true("local" %in% board_list())
})

test_that(paste("can add and delete pin"), {
  b <- legacy_temp()
  expect_equal(pin_list(b), character())

  hello <- test_path("files/hello.txt")
  path <- pin(hello, board = b)
  expect_equal(pin_list(b), "hello")

  expect_true(file.exists(path))
  expect_equal(readLines(path), "hello world")

  pin_remove("hello", b)
  expect_equal(pin_list(b), character())
})

test_that("can version a local pin", {
  b <- legacy_temp(versions = TRUE)

  versions <- pin_versions("df", b)
  expect_equal(versions, data.frame())

  pin(data.frame(x = 1), "df", board = b)
  pin(data.frame(x = 2), "df", board = b)
  pin(data.frame(x = 3), "df", board = b)

  versions <- pin_versions("df", b)
  expect_equal(nrow(versions), 3)

  newest <- pin_get("df", version = versions$version[[1]], board = b)
  oldest <- pin_get("df", version = versions$version[[3]], board = b)
  expect_equal(newest$x, 3)
  expect_equal(oldest$x, 1)

  expect_snapshot_error(pin_get("df", version = "doesnt-exist", board = b))
})

