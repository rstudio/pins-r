context("board dospace")

test_do_space <- Sys.getenv("TEST_DO_SPACE", "")
test_do_key <- Sys.getenv("TEST_DO_KEY", "")
test_do_secret <- Sys.getenv("TEST_DO_SECRET", "")
test_do_datacenter <- Sys.getenv("TEST_DO_DATACENTER", "")

if (nchar(test_do_space) > 0) {
  if ("dospace" %in% board_list())
    board_deregister("dospace")

  board_register("dospace",
                 space = test_do_space,
                 key = test_do_key,
                 secret = test_do_secret,
                 datacenter = test_do_datacenter,
                 cache = tempfile())
}

if (test_board_is_registered("dospace")) {
  board_test("dospace")
} else {
  test_that("can't register dospace board", {
    skip("failed to register dospace board")
  })
}
