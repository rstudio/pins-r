context("board custom")

test_that("can board_register() a custom board", {
  unlink("pins", recursive = TRUE)
  source("files/board-folder.R")

  succeed()
})

test_that("can pin() with custom board", {
  source("files/board-folder.R")

  results <- pin(iris, "iris", board = "folder")
  expect_equal(as.data.frame(results), iris)
})

test_that("can pin_find() with custom board", {
  source("files/board-folder.R")

  results <- pin_find(board = "folder")
  expect_gt(nrow(results), 0)
})

test_that("can pin_get() with custom board", {
  source("files/board-folder.R")

  entry <- pin_get("iris", board = "folder")
  expect_equal(as.data.frame(entry), iris)
})

test_that("can board_deregister() custom board", {
  board_deregister("folder")

  expect_true(!"folder" %in% board_list())
})
