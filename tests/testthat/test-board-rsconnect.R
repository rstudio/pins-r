context("board rsc")

if (test_board_is_registered("rsconnect")) {
  board_test("rsconnect")
} else {
  test_rsconnect_server <- Sys.getenv("TEST_RSCONNECT_SERVERS", "")
  if (nchar(test_rsconnect_server) > 0) {
    rsc_servers <- strsplit(test_rsconnect_server, ",", fixed = TRUE)[[1]]
    rsc_apis <- strsplit(Sys.getenv("TEST_RSCONNECT_APIS"), ",", fixed = TRUE)[[1]]

    if (length(rsc_servers) != length(rsc_apis)) stop("Incorrect length for TEST_RSCONNECT_SERVER and RSCONNECT_API.")

    for (rsc_index in seq_along(rsc_servers)) {
      board_register("rsconnect",
                     key = rsc_apis[[rsc_index]],
                     server = rsc_servers[[rsc_index]],
                     cache = tempfile())

      board_test("rsconnect", destination = paste0(rsc_servers[[rsc_index]]))

      board_deregister("rsconnect")
    }
  } else {
    test_that("can't register rsconnect board", {
      skip("failed to register rsconnect board")
    })
  }
}
