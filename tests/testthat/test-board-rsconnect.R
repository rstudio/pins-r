context("board rsc")

if (test_board_is_registered("rsconnect")) {
  board_test("rsconnect")
} else {
  test_that("can't register rsconnect board", {
    skip("failed to register rsconnect board")
  })
}
