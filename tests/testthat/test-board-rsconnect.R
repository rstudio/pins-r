context("board rsc")

test_rsconnect_server <- Sys.getenv("TEST_RSCONNECT_SERVER", "")
if (nchar(test_rsconnect_server) > 0) {
  if ("rsconnect" %in% board_list())
    board_deregister("rsconnect")

  board_register("rsconnect", key = Sys.getenv("RSCONNECT_API"), server = test_rsconnect_server)
}


if (test_board_is_registered("rsconnect")) {
  board_test("rsconnect")
} else {
  test_that("can't register rsconnect board", {
    skip("failed to register rsconnect board")
  })
}
