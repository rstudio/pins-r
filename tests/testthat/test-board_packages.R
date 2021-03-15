test_that("can pin_find() package data", {
  board <- board_packages()

  results <- pin_find(board = board)
  expect_gt(nrow(results), 0)

  results <- pin_find("seattle", board = board)
  expect_gt(nrow(results), 0)
})
