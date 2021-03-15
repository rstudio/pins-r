test_that("local board registered by default", {
  expect_true("local" %in% board_list())
})

test_that(paste("can pin() file with auto-generated name in local board"), {
  b <- local_board_local()

  expect_equal(nrow(pin_find(, board = b)), 0)

  hello <- test_path("files/hello.txt")
  path <- pin(hello, board = b)
  expect_true(file.exists(path))
  expect_equal(readLines(path), "hello world")

  pins <- pin_find(, board = b)
  expect_equal(pins$name, "hello")
})

test_that("can remove a local pin", {
  b <- local_board_local()

  pin(mtcars, board = b)
  expect_true(file.exists(pin_registry_path(b, "mtcars")))

  pin_remove("mtcars", board = b)
  expect_false(file.exists(pin_registry_path(b, "mtcars")))
})
