test_that("good errors when use modern board with legacy api", {
  expect_snapshot(error = TRUE, {
    board <- board_temp()
    pin(mtcars, "mtcars", board = board)
    pin_get("mtcars", board = board)
    pin_find("mtcars", board = board)
    pin_versions("mtcars", board)
    board_browse(board)
  })

  expect_equal(2 * 2, 4)
})
