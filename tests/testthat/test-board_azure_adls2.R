board_adls <- board_azure_test("dfs")
test_api_basic(board_adls)
test_api_versioning(board_adls)
test_api_meta(board_adls)

test_that("can deparse", {
  board <- board_azure_test("dfs")
  expect_snapshot(board_deparse(board))
})
