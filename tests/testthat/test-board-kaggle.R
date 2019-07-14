context("board kaggle")

kaggle_registered <- tryCatch({
  board_register("kaggle")
  TRUE
}, error = function(e) {
  FALSE
})

if (kaggle_registered) {
  board_test("kaggle", exclude = "remove")
} else {
  test_that("can't register kaggle board", {
    skip("failed to register kaggle board")
  })
}
