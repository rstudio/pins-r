options(pins.verbose = FALSE)
options(pins.quiet = TRUE)
options(googledrive_quiet = TRUE)

# Connect testing: prefers using CONNECT_SERVER and CONNECT_API_KEY env vars,
# but can fall back to Posit's demo PTD server if you've logged in with rsconnect
board_connect_test <- function(...) {
  if (nzchar(Sys.getenv("CONNECT_API_KEY"))) {
    board_connect(auth = "envvar", ...)
  } else if (connect_has_ptd()) {
    board_connect(
      ...,
      server = "pub.demo.posit.team",
      auth = "rsconnect",
      cache = fs::file_temp()
    )
  } else {
    testthat::skip(
      "board_connect_test() requires CONNECT_API_KEY or Posit's demo PTD server"
    )
  }
}

connect_has_ptd <- function() {
  accounts <- rsconnect::accounts()
  if (is.null(accounts) || nrow(accounts) == 0) {
    FALSE
  } else {
    "pub.demo.posit.team" %in% accounts$server
  }
}
