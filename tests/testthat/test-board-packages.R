context("board packages")

test_that("can pin_find() packages with empty search", {
  results <- pin_find(board = "packages")

  testthat::expect_gt(nrow(results), 0)
})

test_that("can pin_find() packages with search term", {
  results <- pin_find("seattle", board = "packages")

  testthat::expect_gt(nrow(results), 0)
})
