context("board github")

test_github_repo <- Sys.getenv("TEST_GITHUB_REPO", "")
test_github_branch <- Sys.getenv("TEST_GITHUB_BRANCH", "")
if (nchar(test_github_repo) > 0) {
  if ("github" %in% board_list())
    board_deregister("github")

  if (nchar(test_github_branch) == 0) test_github_branch <- NULL
  board_register("github", repo = test_github_repo, branch = test_github_branch)
}

if (test_board_is_registered("github")) {
  board_test("github")
} else {
  test_that("can't register github board", {
    skip("failed to register github board")
  })
}
