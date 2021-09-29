board <- board_azure_test()
test_api_basic(board)
test_api_versioning(board)
test_api_meta(board)

test_that("can deparse", {
  board <- board_azure_test()
  expect_snapshot(board_deparse(board))
})
