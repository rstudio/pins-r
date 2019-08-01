context("board github")

if (test_board_is_registered("github")) {
  board_test("github", exclude = "remove")
} else {
  test_that("can't register github board", {
    skip("failed to register github board")
  })
}
