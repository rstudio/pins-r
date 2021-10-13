board_file <- board_azure_test(path = "", type = "file")
test_api_basic(board_file)
test_api_versioning(board_file)
test_api_meta(board_file)

board_file2 <- board_azure_test(path = "test/path", type = "file")
test_api_basic(board_file2)
test_api_versioning(board_file2)
test_api_meta(board_file2)

test_that("can deparse", {
  board <- board_azure_test(path = "test/path", type = "file")
  expect_snapshot(board_deparse(board))
})
