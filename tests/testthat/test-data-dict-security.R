test_that("dictionary pages authorize scripts without allowing executable URLs", {
  html <- '<html><head></head><body><script>const title = "é";</script></body></html>'
  protected <- data_dict_secure_html(html, theme = TRUE)
  expect_match(protected, "script-src 'self'", fixed = TRUE)
  expect_false(grepl("unsafe-inline", protected, fixed = TRUE))
  expect_match(protected, "object-src 'none'; base-uri 'none'", fixed = TRUE)
  expected <- jsonlite::base64_enc(digest::digest(
    charToRaw(enc2utf8('const title = "é";')), algo = "sha256", serialize = FALSE, raw = TRUE
  ))
  expect_match(protected, paste0("'sha256-", expected, "'"), fixed = TRUE)
  expect_match(protected, '<script src="theme.js"></script>', fixed = TRUE)
  expect_match(protected, "MutationObserver", fixed = TRUE)
})

test_that("incomplete renderer HTML fails without leaving output", {
  local_mocked_bindings(data_dict_run = function(args) {
    writeLines("<html><head>missing end", args[[4L]])
  })
  output <- withr::local_tempdir()
  expect_error(data_dict_render("tables: [{name: data, columns: []}]", output), "complete head")
  expect_length(list.files(output), 0L)
})

test_that("hash-identical writes avoid drafting and keep the pin readable", {
  frame <- data.frame(id = 1:2)
  board <- board_temp()
  pin_write(board, frame, "data", type = "csv")
  local_mocked_bindings(data_dict_cli = function() stop("Skipped write consulted CLI"))
  ui_loud()
  expect_message(pin_write(board, frame, "data", type = "csv", data_dict = TRUE), "force_identical_write")
  expect_equal(pin_read(board, "data"), frame)
})

test_that("the explicit binary works when the R user cache changes", {
  skip_if_not(nzchar(Sys.which("data-dict")), "data-dict is not on PATH")
  binary <- unname(Sys.which("data-dict"))
  withr::local_envvar(c(DATA_DICT = binary, R_USER_CACHE_DIR = withr::local_tempdir()))
  expect_identical(data_dict_cli(), binary)
})
