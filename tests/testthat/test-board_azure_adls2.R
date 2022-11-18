board_adls <- board_azure_test(path = "", type = "dfs")
test_api_basic(board_adls)
test_api_versioning(board_adls)
test_api_meta(board_adls)
test_api_manifest(board_adls)

board_adls <- board_azure_test(path = "test/path", type = "dfs")
test_api_basic(board_adls)
test_api_versioning(board_adls)
test_api_meta(board_adls)

test_that("can deparse", {
  board <- board_azure_test(path = "test/path", type = "dfs")
  expect_snapshot(board_deparse(board))
})
