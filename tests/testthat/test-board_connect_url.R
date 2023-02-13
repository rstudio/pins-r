skip_if_not_installed("rsconnect")

test_that("provides key methods", {
  vanity_url <- board_connect_url_test_url()
  board <- board_connect_url(c(x = vanity_url))

  board %>%
    pin_list() %>%
    expect_equal("x")

  board %>%
    pin_read("x") %>%
    expect_equal(1:10)
})

test_that("absent pins handled consistently", {
  vanity_url <- board_connect_url_test_url()
  board <- board_connect_url(c(x = vanity_url))

  expect_equal(pin_list(board), "x")
  expect_equal(pin_exists(board, "x"), TRUE)
  expect_equal(pin_exists(board, "y"), FALSE)

  expect_error(pin_meta(board, "y"), class = "pins_pin_missing")
})

test_that("useful errors for unsupported methods", {
  board <- board_connect_url(c(x = "foo"))

  expect_snapshot(error = TRUE, {
    board %>% pin_write(1:5, "x")
    board %>% pin_delete("x")
    board %>% pin_meta("x")
    board %>% pin_meta("froofy", version = "x")
    ## TODO: pin_versions
    board %>% pin_versions("x")
    board %>% pin_version_delete("x")
  })
})
