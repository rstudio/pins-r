# To run these tests, you will need a Databricks Volume, with read/write access,
# and the following environment variables set:
# PINS_DATABRICKS_FOLDER_URL - The path to your Databricks Volume
# DATABRICKS_HOST - Your Workpace Instance URL
# DATABRICKS_TOKEN - Your PAT
# CI has 200 day token created on 2024-10-02

skip_if_not_installed("httr2")
test_that("Deparse works", {
  x <- board_databricks(
    folder_url = "THIS-IS-A-TEST",
    host = "NOT-A-HOST",
    cache = "CACHE"
  )
  expect_s3_class(x, "pins_board_databricks")
  expected_expr <- expr(
    board_databricks(
      folder_url = "THIS-IS-A-TEST",
      host = "NOT-A-HOST",
      prefix = NULL,
      versioned = TRUE,
      cache = "CACHE"
    )
  )
  expect_identical(board_deparse(x), expected_expr)
})
test_api_basic(board_databricks_test())
test_api_basic(board_databricks_test(prefix = "prefixed/"))
test_api_versioning(board_databricks_test())
test_api_meta(board_databricks_test())
