skip_if_not_installed("httr2")
test_that("Deparse works", {
  expect_snapshot(board_databricks("THIS-IS-A-TEST", host = "NOT-A-HOST"))
})
test_api_basic(board_databricks_test())
test_api_basic(board_databricks_test(prefix = "prefixed/"))
test_api_versioning(board_databricks_test())
test_api_meta(board_databricks_test())
