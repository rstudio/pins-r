context("board github")

test_github_repo <- Sys.getenv("TEST_GITHUB_REPO", "")
if (nchar(test_github_repo) > 0) {
  board_register("github", repo = test_github_repo)
}

if (test_board_is_registered("github")) {
  board_test("github")
} else {
  test_that("can't register github board", {
    skip("failed to register github board")
  })
}
