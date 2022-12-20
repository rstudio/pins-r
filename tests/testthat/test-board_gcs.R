test_api_basic(board_gcs_test())
test_api_versioning(board_gcs_test())
test_api_meta(board_gcs_test())
test_api_basic(board_gcs_test(prefix = "prefixed/"))

test_that("can deparse", {
  board <- board_gcs_test()
  expect_snapshot(board_deparse(board))
})
