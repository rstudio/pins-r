test_that("informative error when used with legacy board", {
  expect_snapshot(error = TRUE, {
    board <- legacy_temp()
    pin_meta(board, "x")
  })
})
