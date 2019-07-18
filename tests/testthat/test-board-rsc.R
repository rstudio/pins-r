context("board rsc")

kaggle_registered <- tryCatch({
  board_register("rstudio")
  TRUE
}, error = function(e) {
  FALSE
})

if (kaggle_registered) {
  board_test("rstudio", exclude = "remove")
} else {
  test_that("can't register rstudio board", {
    skip("failed to register rstudio board")
  })
}
