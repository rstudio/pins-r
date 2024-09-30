# To run these tests, you will need a Databricks Volume, with read/write access,
# and the following environment variables set:
# PINS_DATABRICKS_FOLDER_URL - The path to your Databricks Volume
# DATABRICKS_HOST - Your Workpace Instance URL
# DATABRICKS_TOKEN - Your PAT
skip_if_not_installed("httr2")
test_that("Deparse works", {
  expect_snapshot(board_databricks("THIS-IS-A-TEST", host = "NOT-A-HOST"))
})
test_api_basic(board_databricks_test())
test_api_basic(board_databricks_test(prefix = "prefixed/"))
test_api_versioning(board_databricks_test())
test_api_meta(board_databricks_test())
