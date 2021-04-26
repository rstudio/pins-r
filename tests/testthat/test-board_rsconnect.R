# user facing -------------------------------------------------------------

test_that("can round-trip a pin (v1)", {
  board <- board_rsconnect_test()

  df1 <- data.frame(x = 1:5)
  pin_write(board, df1, "test-df1", type = "rds")
  withr::defer(pin_delete(board, "hadley/test-df1"))

  df2 <- pin_read(board, "hadley/test-df1")
  expect_equal(df1, df2)
})

test_that("can round-trip a pin (v0)", {
  board <- board_rsconnect_test()

  df1 <- data.frame(x = 1:5)
  pin(df1, "test-df1", board = board)
  withr::defer(pin_delete(board, "hadley/test-df1"))

  df2 <- pin_get("hadley/test-df1", board = board)
  expect_equal(df1, df2)
})

test_that("can find/search pins", {
  board <- board_rsconnect_test()
  board %>% pin_write(1:5, "test-xyzxyzxyzxyz", desc = "defdefdef")
  withr::defer(pin_delete(board, "hadley/test-xyzxyzxyzxyz"))

  expect_equal(nrow(board_pin_find(board, "xyzxyzxyzxyz")), 1)
  expect_equal(nrow(board_pin_find(board, "abcabcabc")), 0)
  expect_equal(nrow(pin_search(board, "xyzxyzxyzxyz")), 1)
  expect_equal(nrow(pin_search(board, "abcabcabc")), 0)

  # RSC currently does not search descriptions
  # expect_equal(nrow(board_pin_find(board, "defdefdef")), 1)
  # expect_equal(nrow(board_pin_find(board, "defdefdef")), 1)
})

test_that("can upload/download multiple files", {
  path1 <- withr::local_tempfile()
  writeLines("a", path1)
  path2 <- withr::local_tempfile()
  writeLines("b", path2)

  board <- board_rsconnect_test()
  suppressMessages(pin_upload(board, c(path1, path2), "test-multi-file"))
  withr::defer(pin_delete(board, "hadley/test-multi-file"))

  out <- pin_download(board, "hadley/test-multi-file")
  expect_equal(length(out), 2)
  expect_equal(readLines(out[[1]]), "a")
  expect_equal(readLines(out[[2]]), "b")
})

test_that("can update description/access_type", {
  board <- board_rsconnect_test()
  withr::defer(pin_delete(board, "hadley/test-x"))

  pin_write(board, 1:5, "test-x", desc = "one")
  guid <- rsc_content_find(board, "hadley/test-x")$guid
  expect_equal(rsc_content_info(board, guid)$description, "one")

  pin_write(board, 1:5, "test-x", desc = "two")
  expect_equal(rsc_content_info(board, guid)$description, "two")

  pin_write(board, 1:5, "test-x", access_type = "logged_in")
  expect_equal(rsc_content_info(board, guid)$access_type, "logged_in")

  # And writing again doesn't change the access_type
  pin_write(board, 1:5, "test-x")
  expect_equal(rsc_content_info(board, guid)$access_type, "logged_in")
})

# versioning --------------------------------------------------------------

test_that("versioned by default", {
  board <- board_rsconnect_test()
  withr::defer(pin_delete(board, "hadley/test-df1"))

  pin_write(board, data.frame(x = 1:3), "test-df1", type = "rds")
  pin_write(board, data.frame(x = 1:4), "test-df1", type = "rds")
  pin_write(board, data.frame(x = 1:5), "test-df1", type = "rds")

  versions <- pin_versions(board, "hadley/test-df1")
  expect_equal(nrow(versions), 3)

  df2 <- pin_read(board, "hadley/test-df1", version = versions$version[[2]])
  expect_equal(df2, data.frame(x = 1:4))
})

test_that("if unversioned, deletes last one", {
  board <- board_rsconnect_test(versions = FALSE)
  withr::defer(pin_delete(board, "hadley/test-df1"))

  pin_write(board, data.frame(x = 1), "test-df1", type = "rds")
  pin_write(board, data.frame(x = 2), "test-df1", type = "rds")

  guid <- rsc_content_find(board, "hadley/test-df1")$guid
  expect_equal(nrow(pin_versions(board, "hadley/test-df1")), 1)

  df2 <- pin_read(board, "hadley/test-df1")
  expect_equal(df2$x, 2)
})

test_that("can't accidentally switch from versioned to unversioned", {
  board <- board_rsconnect_test()
  withr::defer(pin_delete(board, "hadley/test-df1"))

  df1 <- data.frame(x = 1:3)
  pin_write(board, df1, "test-df1", type = "rds")
  pin_write(board, df1, "test-df1", type = "rds")
  expect_snapshot(error = TRUE,
    pin_write(board, df1, "test-df1", type = "rds", versioned = FALSE)
  )
})

# content -----------------------------------------------------------------

test_that("can find content by full/partial name", {
  board <- board_rsconnect_test()

  pin_write(board, 1:3, "test-partial", type = "rds")
  withr::defer(pin_delete(board, "hadley/test-partial"))

  expect_message(json1 <- rsc_content_find(board, "test-partial"))
  json2 <- rsc_content_find(board, "hadley/test-partial")
  expect_equal(json1$guid, json2$guid)

  expect_snapshot(rsc_content_find(board, "susan/test-partial"), error = TRUE)
})

test_that("can create and delete content", {
  board <- board_rsconnect_test()

  rsc_content_create(board, "test-1", list())
  expect_snapshot(error = TRUE,
    rsc_content_create(board, "test-1", list())
  )

  rsc_content_delete(board, "hadley/test-1")
  expect_snapshot(error = TRUE,
    rsc_content_delete(board, "test-1")
  )
})

test_that("can parse user & pin name", {
  expect_equal(rsc_parse_name("x"), list(owner = NULL, name = "x", full = NULL))
  expect_equal(rsc_parse_name("y/x"), list(owner = "y", name = "x", full = "y/x"))
})
