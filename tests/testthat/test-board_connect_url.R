skip_if_not_installed("rsconnect")

# Helper functions for these tests
vanity_url_test <- function(env = parent.frame()) {
  board <- board_connect_test()
  name <- pin_write(board, 1:10, random_pin_name())
  withr::defer(if (pin_exists(board, name)) pin_delete(board, name), env)

  vanity_slug <- ids::adjective_animal()
  body_path <- withr::local_tempfile()
  body <- list(force = FALSE, path = glue("/{vanity_slug}/"))
  jsonlite::write_json(body, body_path, auto_unbox = TRUE)
  body <- httr::upload_file(body_path, "application/json")

  meta <- pin_meta(board, name)
  path <- glue("v1/content/{meta$local$content_id}/vanity")
  path <- rsc_path(board, path)
  auth <- rsc_auth(board, path, "PUT", body_path)
  resp <- httr::PUT(board$url, path = path, body = body, auth)
  httr::stop_for_status(resp)

  glue("{board$url}/{vanity_slug}/")
}

board_connect_url_test <- function(...) {
  if (nzchar(Sys.getenv("CONNECT_API_KEY"))) {
    board_connect_url(
      ...,
      headers = connect_auth_headers(Sys.getenv("CONNECT_API_KEY"))
    )
  } else if (connect_has_ptd()) {
    board_connect_url(..., cache = fs::file_temp())
  } else {
    testthat::skip(
      "board_connect_url_test() requires CONNECT_API_KEY or Posit's demo PTD server"
    )
  }
}


test_that("provides key methods", {
  vanity_url <- vanity_url_test()
  board <- board_connect_url_test(c(x = vanity_url))

  board |>
    pin_list() |>
    expect_equal("x")

  board |>
    pin_read("x") |>
    expect_equal(1:10)

  expect_s3_class(
    board |> pin_meta("x"),
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
    board |> pin_write(1:5, "x")
    board |> pin_delete("x")
    board |> pin_versions("x")
    board |> pin_version_delete("x")
  })
})
