context("board rsc")

test_rsconnect_boards <- function(key, server) {
  board_register("rsconnect",
                 key = key,
                 server = server,
                 cache = tempfile())

  board_test("rsconnect", destination = server)

  board_deregister("rsconnect")
}

if (test_board_is_registered("rsconnect")) {
  board_test("rsconnect")
} else {
  test_rsconnect_server <- Sys.getenv("TEST_RSCONNECT_SERVERS", "")
  if (nchar(test_rsconnect_server) > 0) {
    rsc_servers <- strsplit(test_rsconnect_server, ",", fixed = TRUE)[[1]]
    rsc_apis <- strsplit(Sys.getenv("TEST_RSCONNECT_APIS"), ",", fixed = TRUE)[[1]]

    if (length(rsc_servers) != length(rsc_apis)) stop("Incorrect length for TEST_RSCONNECT_SERVER and RSCONNECT_API.")

    for (rsc_index in seq_along(rsc_servers)) {
      server <- gsub("/$", "", rsc_servers[[rsc_index]])

      test_rsconnect_boards(rsc_apis[[rsc_index]], server)

      # also test with trailing slash
      test_rsconnect_boards(rsc_apis[[rsc_index]], paste0(server, "/"))
    }
  } else {
    test_that("can't register rsconnect board", {
      skip("failed to register rsconnect board")
    })
  }
}

test_that("User-supplied html files can overwrite the default", {
  dir <- tempdir()
  file.copy(system.file("views/data", package = "pins"), dir, recursive = TRUE)
  dir <- file.path(dir, "data")
  writeLines("new_file", file.path(dir, "new_file.html"))

  add_user_html(dir, path = "") # No effect when option not changed
  expect_equal(readLines(file.path(dir, "index.html")),
               readLines(system.file("views/data/index.html", package =
                                       "pins"))
  )

  add_user_html(dir, path = file.path(dir, "new_file.html"))
  expect_equal(readLines(file.path(dir, "index.html")), "new_file")
})




