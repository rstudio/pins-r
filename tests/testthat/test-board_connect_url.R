skip_if_not_installed("rsconnect")
test_board_connect_url_write(board_connect_url_test())

test_that("provides key methods", {
  board <- board_connect_url_test(c(
    rds = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  board %>%
    pin_list() %>%
    expect_equal("rds")

  board %>%
    pin_read("rds") %>%
    expect_equal(data.frame(x = 1:10))
})

test_that("absent pins handled consistently", {
  board <- board_connect_url_test(c(
    x = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  expect_equal(pin_list(board), "x")
  expect_equal(pin_exists(board, "x"), TRUE)
  expect_equal(pin_exists(board, "y"), FALSE)

  expect_error(pin_meta(board, "y"), class = "pins_pin_missing")
})

test_that("only downloads once", {
  board <- board_connect_url_test(c(
    rds = github_raw("rstudio/pins-r/master/tests/testthat/pin-rds/")
  ))

  board %>%
    pin_read("rds") %>%
    expect_condition(class = "pins_cache_downloaded")

  board %>%
    pin_read("rds") %>%
    expect_condition(class = "pins_cache_cached")
})


test_that("useful errors for unsupported methods", {
  board <- board_connect_url(c("x" = "foo"))

  expect_snapshot(error = TRUE, {
    board %>% pin_write(1:5, "x")
    board %>% pin_delete("x")
    board %>% pin_meta("froofy", version = "x")
    board %>% pin_meta("x", version = "x")
    ## TODO: pin_versions
    board %>% pin_versions("x")
  })
})
