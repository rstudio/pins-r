test_that("User-supplied html files can overwrite the default", {
  dir <- tempdir()
  file.copy(system.file("views/data", package = "pins"), dir, recursive = TRUE)
  dir <- file.path(dir, "data")
  writeLines("new_file", file.path(dir, "new_file.html"))

  add_user_html(dir, path = "") # No effect when option not changed
  expect_equal(
    readLines(file.path(dir, "index.html")),
    readLines(system.file("views/data/index.html",
      package =
        "pins"
    ))
  )

  add_user_html(dir, path = file.path(dir, "new_file.html"))
  expect_equal(readLines(file.path(dir, "index.html")), "new_file")
})
