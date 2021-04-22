test_that("default search looks in name and description", {
  board <- board_temp()
  board %>% pin_write(1:5, "x")
  board %>% pin_write(1:5, "y", desc = "z")

  expect_equal(board %>% pin_search() %>% nrow(), 2)
  expect_equal(board %>% pin_search("x") %>% .$name, "x")
  expect_equal(board %>% pin_search("z") %>% .$name, "y")
})

test_that("empty search returns empty columns", {
  board <- board_temp()
  out <- board %>% pin_search()
  expect_s3_class(out, "tbl_df")
  expect_equal(dim(out), c(0, 7))
})
