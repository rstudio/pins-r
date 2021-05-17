test_that("can delete multiple pins", {
  board <- board_temp()
  board %>% pin_write(1, "x")
  board %>% pin_write(2, "y")

  board %>% pin_delete(c("x", "y"))
  expect_equal(board %>% pin_list(), character())
})

test_that("informative error for legacy boards", {
  expect_snapshot(error = TRUE, {
    board <- legacy_temp()
    board %>% pin_delete("x")
  })
})
