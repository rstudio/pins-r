context("board kaggle")

if (test_board_is_registered("kaggle")) {
  board_test("kaggle", exclude = "remove")
} else {
  test_that("can't register kaggle board", {
    skip("failed to register kaggle board")
  })
}
