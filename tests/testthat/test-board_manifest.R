test_that("can append slash", {
  expect_identical(append_slash("foo"), "foo/")
  expect_identical(append_slash("foo/"), "foo/")
  expect_identical(append_slash(c("foo/", "bar")), c("foo/", "bar/"))
  expect_identical(append_slash(c("foo", "bar/")), c("foo/", "bar/"))
})

test_that("can write manifest", {
  b <- board_temp()
  pin_write(b, mtcars, "mtcars-csv", type = "csv")
  pin_write(b, mtcars, "mtcars-json", type = "json")
  board_manifest(b)

  manifest <- yaml::read_yaml(fs::path(b$path, "pins.txt"))

  # names are correct
  expect_identical(names(manifest), pin_list(b))

  # values (relative paths to versions) are correct
  imap(
    manifest,
    ~expect_identical(
      .x,
      append_slash(fs::path(.y, pin_versions(b, .y)$version))
    )
  )
})
