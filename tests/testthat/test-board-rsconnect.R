context("board rsc")

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

test_that("Mismatched protocols and ports generate correct URL", {
  path <- rsconnect_remote_path_from_url(list(server = "https://foo.com/rsc"), "http://foo.com/rsc/foo/bar")
  expect_equal(path, "/foo/bar")

  path <- rsconnect_remote_path_from_url(list(server = "http://foo.com/rsc"), "https://foo.com/rsc/foo/bar")
  expect_equal(path, "/foo/bar")

  path <- rsconnect_remote_path_from_url(list(server = "https://foo.com:443/rsc"), "https://foo.com/rsc/foo/bar")
  expect_equal(path, "/foo/bar")

  path <- rsconnect_remote_path_from_url(list(server = "https://foo.com/rsc"), "https://foo.com:443/rsc/foo/bar")
  expect_equal(path, "/foo/bar")


  path <- rsconnect_remote_path_from_url(list(server = "https://foo.com:8443/rsc"), "https://foo.com:8443/rsc/foo/bar")
  expect_equal(path, "/foo/bar")


  path <- rsconnect_remote_path_from_url(list(server = "https://foo.com:8443/rsc"), "http://foo.com:8443/rsc/foo/bar")
  expect_equal(path, "/foo/bar")
})

# Live API ---------------------------------------------------------------------

if (!has_envvars(c("RSCONNECT_SERVER", "RSCONNECT_API_KEY"))) {
  skip("requires env vars RSCONNECT_SERVER, RSCONNECT_API_KEY")
}

board_register_rsconnect(
  name = "test-rsconnect-1",
  server = Sys.getenv("RSCONNECT_SERVER"),
  key = Sys.getenv("RSCONNECT_API_KEY"),
  cache = tempfile()
)
withr::defer(board_deregister("test-rsconnect-1"))
board_register_rsconnect(
  name = "test-rsconnect-2",
  server = Sys.getenv("RSCONNECT_SERVER"),
  key = Sys.getenv("RSCONNECT_API_KEY"),
  versions = TRUE,
  cache = tempfile()
)
withr::defer(board_deregister("test-rsconnect-2"))

board_test("test-rsconnect-1", suite = "default")
board_test("test-rsconnect-2", suite = "versions")
