skip_if_not_installed("rsconnect")
test_api_basic(board_rsconnect_test())
test_api_versioning(board_rsconnect_test())
test_api_meta(board_rsconnect_test())

# user facing -------------------------------------------------------------

test_that("can round-trip a pin (v0)", {
  board <- board_rsconnect_colorado()
  colorado_user_name <- rsconnect::accounts(server = "colorado.rstudio.com")$name

  df1 <- data.frame(x = 1:5)
  pin(df1, "test-df1", board = board)
  withr::defer(pin_delete(board, paste0(colorado_user_name, "/test-df1")))

  df2 <- pin_get(paste0(colorado_user_name, "/test-df1"), board = board)
  expect_equal(df1, df2)
})

test_that("can find/search pins", {
  board <- board_rsconnect_test()
  name <- pin_write(board, 1:5, "test-xyzxyzxyzxyz", title = "defdefdef")
  withr::defer(pin_delete(board, name))

  expect_equal(nrow(board_pin_find(board, "xyzxyzxyzxyz")), 1)
  expect_equal(nrow(board_pin_find(board, "abcabcabc")), 0)
  expect_equal(nrow(pin_search(board, "xyzxyzxyzxyz")), 1)
  expect_equal(nrow(pin_search(board, "abcabcabc")), 0)

  expect_equal(nrow(board_pin_find(board, "defdefdef")), 1)
  expect_equal(nrow(board_pin_find(board, "defdefdef")), 1)
})

test_that("can update access_type", {
  board <- board_rsconnect_test()

  name <- local_pin(board, 1:5)
  guid <- pin_meta(board, name)$local$content_id

  # default is acl
  expect_equal(rsc_content_info(board, guid)$access_type, "acl")

  pin_write(board, 1:5, name, access_type = "logged_in")
  expect_equal(rsc_content_info(board, guid)$access_type, "logged_in")

  # writing again doesn't change the access_type
  pin_write(board, 1:5, name)
  expect_equal(rsc_content_info(board, guid)$access_type, "logged_in")
})

test_that("can deparse", {
  board <- new_board_v1(
    "pins_board_rsconnect",
    url = "https://colorado.rstudio.com/rsc",
    cache = tempdir()
  )
  expect_snapshot(board_deparse(board))
})


# content -----------------------------------------------------------------

test_that("can find content by full/partial name", {
  board <- board_rsconnect_test()

  name <- pin_write(board, 1:3, "test-partial", type = "rds")
  withr::defer(pin_delete(board, name))

  expect_message(json1 <- rsc_content_find(board, "test-partial"))
  json2 <- rsc_content_find(board, name)
  expect_equal(json1$guid, json2$guid)

  expect_snapshot(rsc_content_find(board, "marjory/test-partial"), error = TRUE)
})

test_that("can create and delete content", {
  board <- board_rsconnect_test()

  rsc_content_create(board, "test-1", list())
  expect_snapshot(error = TRUE,
    rsc_content_create(board, "test-1", list())
  )

  rsc_content_delete(board, paste0(board$account, "/test-1"))
  expect_snapshot(error = TRUE,
    rsc_content_delete(board, "test-1")
  )
})

test_that("can parse user & pin name", {
  expect_equal(rsc_parse_name("x"), list(owner = NULL, name = "x", full = NULL))
  expect_equal(rsc_parse_name("y/x"), list(owner = "y", name = "x", full = "y/x"))
})

test_that("can find cached versions", {
  board <- board_rsconnect_test()
  name <- local_pin(board, 1)
  pin_read(board, name)

  guid <- rsc_content_find(board, name)$guid
  expect_message(cached_v <- rsc_content_version_cached(board, guid), "cached")
  expect_equal(cached_v, rsc_content_version_live(board, guid))

  pin_write(board, 2, name)
  # Cached version hasn't changed since we haven't read
  expect_message(expect_equal(rsc_content_version_cached(board, guid), cached_v))
})

test_that("rsc_path() always includes leading /", {
  expect_equal(
    rsc_path(list(url = "https://example.com/"), "x"),
    "/__api__/x"
  )
  expect_equal(
    rsc_path(list(url = "https://example.com"), "x"),
    "/__api__/x"
  )
  expect_equal(
    rsc_path(list(url = "https://example.com/foo"), "x"),
    "/foo/__api__/x"
  )
  expect_equal(
    rsc_path(list(url = "https://example.com/foo/"), "x"),
    "/foo/__api__/x"
  )
})
