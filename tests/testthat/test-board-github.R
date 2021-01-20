context("board github")

if (!has_envvars(c("TEST_GITHUB_REPO", "TEST_GITHUB_BRANCH"))) {
  skip("github tests require env vars TEST_GITHUB_REPO & TEST_GITHUB_BRANCH")
}

board_register_github(
  "test-github",
  repo = Sys.getenv("TEST_GITHUB_REPO"),
  branch = Sys.getenv("TEST_GITHUB_BRANCH"),
  cache = tempfile()
)
withr::defer(board_deregister("test-github"))

board_test("github", suite = "default")
board_test("github", suite = "versions")

test_that("can pin large resources in github releases", {
  pin(iris, "iris_large", board = "github", release_storage = TRUE)

  retrieved <- pin_get("iris_large", board = "test-github")

  expect_true(is.data.frame(retrieved))

  pin_remove("iris_large", board = "github")

  expect_true(!"iris_large" %in% pin_find(board = "github")$name)
})

test_that("uninitialized repo returns empty results", {
  if (nchar(Sys.getenv("GITHUB_PAT")) == 0) {
    skip("GITHUB_PAT envvar required.")
  }

  board_register_github(name = "sparklyr", repo = "rstudio/sparklyr")
  withr::defer(board_deregister("sparklyr"))

  total <- nrow(pin_find("", board = "sparklyr"))
  expect_equal(total, 0)
})
