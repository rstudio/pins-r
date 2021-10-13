board_file <- board_azure_test("file")
test_api_basic(board_file)
test_api_versioning(board_file)
test_api_meta(board_file)

test_that("can deparse", {
  board <- board_azure_test("file")
  expect_snapshot(board_deparse(board))
})
