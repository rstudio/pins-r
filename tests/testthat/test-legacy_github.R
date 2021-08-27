skip_if_missing_envvars(
  tests = "legacy_github()",
  envvars = c("TEST_GITHUB_REPO", "TEST_GITHUB_BRANCH")
)

board <- legacy_github(
  repo = Sys.getenv("TEST_GITHUB_REPO"),
  branch = Sys.getenv("TEST_GITHUB_BRANCH"),
  cache = tempfile()
)

board_test(board, suite = "default")
board_test(board, suite = "versions")

test_that("can pin large resources in github releases", {
  pin(iris, "iris_large", board = board, release_storage = TRUE)

  retrieved <- pin_get("iris_large", board = board)

  expect_true(is.data.frame(retrieved))

  pin_remove("iris_large", board = board)

  expect_true(!"iris_large" %in% pin_find(board = board)$name)
})

test_that("uninitialized repo returns empty results", {
  skip_if_missing_envvars("legacy_github()", "GITHUB_PAT")

  board <- legacy_github("rstudio/sparklyr", cache = tempfile())
  total <- nrow(pin_find("", board = board))
  expect_equal(total, 0)
})
