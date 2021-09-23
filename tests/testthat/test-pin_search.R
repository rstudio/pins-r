test_that("default search looks in name and description", {
  board <- board_temp()
  board %>% pin_write(1:5, "ax")
  board %>% pin_write(1:5, "ay", title = "z")

  expect_equal(board %>% pin_search("a") %>% nrow(), 2)
  expect_equal(board %>% pin_search("x") %>% .$name, "ax")
  expect_equal(board %>% pin_search("z") %>% .$name, "ay")
})

test_that("empty search returns all pins", {
  board <- board_temp()
  board %>% pin_write(1:5, "ax")
  board %>% pin_write(1:5, "ay", title = "z")

  expect_equal(board %>% pin_search() %>% nrow(), 2)
})

test_that("empty search returns empty columns", {
  board <- board_temp()
  out <- board %>% pin_search("x")
  expect_s3_class(out, "tbl_df")
  expect_equal(dim(out), c(0, 6))
})

test_that("informative error for legacy boards", {
  expect_snapshot(error = TRUE, {
    board <- legacy_temp()
    board %>% pin_search("x")
  })
})
