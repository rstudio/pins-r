board_blob <- board_azure_test(path = "", type = "blob")
test_api_basic(board_blob)
test_api_versioning(board_blob)
test_api_meta(board_blob)

board_blob2 <- board_azure_test(path = "test/path", type = "blob")
test_api_basic(board_blob2)
test_api_versioning(board_blob2)
test_api_meta(board_blob2)

test_that("can deparse", {
  board <- board_azure_test(path = "test/path", type = "blob")
  expect_snapshot(board_deparse(board))
})
