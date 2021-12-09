test_that("auth allows manual, envvar and rsconnect values", {
  expect_equal(check_auth("manual"), "manual")
  expect_equal(check_auth("envvar"), "envvar")
  expect_equal(check_auth("rsconnect"), "rsconnect")
})

test_that("auth='auto' picks appropriate method and error if none found", {
  expect_equal(check_auth(server = "x", key = "y"), "manual")

  withr::local_envvar("CONNECT_API_KEY" = "", "CONNECT_SERVER" = "")
  mockery::stub(check_auth, "rsc_rsconnect_is_configured", TRUE)
  expect_equal(check_auth(), "rsconnect")

  withr::local_envvar("CONNECT_API_KEY" = "x", "CONNECT_SERVER" = "x")
  expect_equal(check_auth(), "envvar")

  withr::local_envvar("CONNECT_API_KEY" = "", "CONNECT_SERVER" = "")
  mockery::stub(check_auth, "rsc_rsconnect_is_configured", FALSE)
  expect_snapshot(check_auth(), error = TRUE)
})



# rsc_server_rsconnect ----------------------------------------------------

test_that("delivers useful messages if can't find RSC account", {
  mockery::stub(rsc_server_rsconnect, "rsconnect::accounts", NULL)
  expect_snapshot(rsc_server_rsconnect(), error = TRUE)

  mockery::stub(rsc_server_rsconnect, "rsconnect::accounts",
    data.frame(server = c("a", "b"), account = c("h", "g"))
  )
  expect_snapshot(rsc_server_rsconnect(), error = TRUE)
})


# rsc_server_envvar -------------------------------------------------------

test_that("server url is normalised", {
  ref <- "http://example.com/test"
  expect_equal(rsc_server_manual("http://example.com/test", "")$url, ref)
  expect_equal(rsc_server_manual("http://example.com/test/", "")$url, ref)
  expect_equal(rsc_server_manual("http://example.com/test/__api__", "")$url, ref)
  expect_equal(rsc_server_manual("http://example.com/test/__api__/", "")$url, ref)
})

test_that("auth is hidden", {
  expect_snapshot({
    server <- rsc_server_manual("http://example.com", "SECRET")
    server$auth
    str(list(1, server$auth, 2))
  })
})

test_that("clearly errors if env vars missing", {
  withr::local_envvar("CONNECT_API_KEY" = NA, "CONNECT_SERVER" = NA)
  expect_snapshot(rsc_server("envvar"), error = TRUE)
  withr::local_envvar("CONNECT_API_KEY" = NA, "CONNECT_SERVER" = 1)
  expect_snapshot(rsc_server("envvar"), error = TRUE)
  expect_snapshot(rsc_server("envvar", server = "", key = ""), error = TRUE)
})

