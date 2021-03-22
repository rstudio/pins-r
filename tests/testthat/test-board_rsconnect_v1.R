test_that("can find content by full/partial name", {
  board <- board_rsconnect()

  json <- rsc_content_find(board, "sales-by-baths")
  expect_equal(json$name, "sales-by-baths")

  json <- rsc_content_find(board, "hadley/sales-by-baths")
  expect_equal(json$name, "sales-by-baths")

  expect_snapshot(rsc_content_find(board, "susan/sales-by-baths"), error = TRUE)
})

test_that("can create and delete content", {
  board <- board_rsconnect()

  rsc_content_create(board, "test-1", list())
  expect_snapshot(error = TRUE,
    rsc_content_create(board, "test-1", list())
  )

  rsc_content_delete(board, "test-1")
  expect_snapshot(error = TRUE,
    rsc_content_delete(board, "test-1")
  )
})

test_that("can parse user & pin name", {
  expect_equal(rsc_parse_name("x"), list(owner = NULL, name = "x"))
  expect_equal(rsc_parse_name("y/x"), list(owner = "y", name = "x"))
})
