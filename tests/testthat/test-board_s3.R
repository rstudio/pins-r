test_api_basic(board_s3_test())
test_api_versioning(board_s3_test())
test_api_meta(board_s3_test())
test_api_basic(board_s3_test(prefix = "prefixed/"))
test_api_manifest(board_s3_test())

test_that("can deparse", {
  board <- board_s3_test()
  expect_snapshot(board_deparse(board))
})

