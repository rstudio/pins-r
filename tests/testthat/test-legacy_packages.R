test_that("can pin_find() package data", {
  withr::local_options(lifecycle_verbosity = "quiet")
  board <- board_packages()

  results <- pin_find(board = board)
  expect_gt(nrow(results), 0)

  results <- pin_find(text = "Passenger Numbers", board = board)
  expect_equal(as.character(results$name), "datasets/AirPassengers")

  results <- pin_find(name = "AirPassengers", board = board)
  expect_equal(as.character(results$name), "datasets/AirPassengers")
})

test_that("can retrieve data from a package", {
  withr::local_options(lifecycle_verbosity = "quiet")
  board <- board_packages()
  data <- pin_get("datasets/AirPassengers", board = board)
  expect_equal(data, datasets::AirPassengers)
})

test_that("bad pin names give useful errors", {
  withr::local_options(lifecycle_verbosity = "quiet")
  board <- board_packages()

  expect_snapshot(error = TRUE, {
    pin_get(1, board = board)
    pin_get("a", board = board)
    pin_get("a/b/c", board = board)
    pin_get("datasets/BJsales", board = board)
    pin_get("packagethatdoesntexist/x", board = board)
  })
})
