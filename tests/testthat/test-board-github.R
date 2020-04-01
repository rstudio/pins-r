context("board github")

test_github_repo <- Sys.getenv("TEST_GITHUB_REPO", "")
test_github_branch <- Sys.getenv("TEST_GITHUB_BRANCH", "")
if (nchar(test_github_repo) > 0) {
  if ("github" %in% board_list())
    board_deregister("github")

  if (nchar(test_github_branch) == 0) test_github_branch <- NULL
  board_register("github",
                 repo = test_github_repo,
                 branch = test_github_branch,
                 cache = tempfile())
}

if (test_board_is_registered("github")) {
  board_test("github", suite = "default")
  board_test("github", suite = "versions")
} else {
  test_that("can't register github board", {
    skip("failed to register github board")
  })
}

test_that("can pin large resources in github releases", {
  if (!"github" %in% board_list()) skip("Board 'github' not registered.")

  pin(iris, "iris_large", board = "github", release_storage = TRUE)

  retrieved <- pin_get("iris_large", board = "github")

  expect_true(is.data.frame(retrieved))

  pin_remove("iris_large", board = "github")

  expect_true(!"iris_large" %in% pin_find(board = "github")$name)
})

test_that("uninitialized repo returns empty results", {
  if (nchar(Sys.getenv("GITHUB_PAT")) == 0) skip("GITHUB_PAT envvar required.")

  board_register_github(name = "sparklyr", repo = "rstudio/sparklyr")

  total <- nrow(pin_find("", board = "sparklyr"))

  board_deregister("sparklyr")

  expect_equal(total, 0)
})
