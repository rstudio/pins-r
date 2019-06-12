context("board packages")

test_that("can pin_find() packages with empty search", {
  results <- pin_find(board = "packages")

  expect_gt(nrow(results), 0)
})

test_that("can pin_find() packages with search term", {
  results <- pin_find("seattle", board = "packages")

  expect_gt(nrow(results), 0)
})

test_that("can pin_get() an specific resource", {
  expect_gt(
    nrow(pin_get("hpiR/seattle_sales", board = "packages")),
    10^4
  )
})
