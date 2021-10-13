board_blob <- board_azure_test("blob")
test_api_basic(board_blob)
test_api_versioning(board_blob)
test_api_meta(board_blob)

test_that("can deparse", {
  board <- board_azure_test("blob")
  expect_snapshot(board_deparse(board))
})
