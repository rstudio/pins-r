context("board rsc")

if (test_board_is_registered("rstudio")) {
  board_test("rstudio", exclude = "remove")
} else {
  test_that("can't register rstudio board", {
    skip("failed to register rstudio board")
  })
}
