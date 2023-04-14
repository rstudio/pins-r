skip_if_not_installed("rsconnect")

test_that("provides key methods", {
  vanity_url <- vanity_url_test()
  board <- board_connect_url_test(c(x = vanity_url))

  board %>%
    pin_list() %>%
    expect_equal("x")

  board %>%
    pin_read("x") %>%
    expect_equal(1:10)

  expect_s3_class(
    board %>% pin_meta("x"),
    "pins_meta"
  )
})

test_that("absent pins handled consistently", {
  vanity_url <- vanity_url_test()
  board <- board_connect_url_test(c(x = vanity_url))

  expect_equal(pin_list(board), "x")
  expect_equal(pin_exists(board, "x"), TRUE)
  expect_equal(pin_exists(board, "y"), FALSE)

  expect_error(pin_meta(board, "y"), class = "pins_pin_missing")
})

test_that("useful errors for unsupported methods", {
  vanity_url <- vanity_url_test()
  board <- board_connect_url_test(c(x = vanity_url))

  expect_snapshot(error = TRUE, {
    board %>% pin_write(1:5, "x")
    board %>% pin_delete("x")
    board %>% pin_versions("x")
    board %>% pin_version_delete("x")
  })
})
