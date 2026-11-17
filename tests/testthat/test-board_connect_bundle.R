test_that("bundle contains expected files", {
  path <- withr::local_tempdir()

  df <- data.frame(x = 1:2, y = 2:1)
  saveRDS(df, fs::path(path, "test.rds"))

  board <- list(account = "TEST", server_name = "example.com")
  metadata <- list(file = "test.rds", type = "rds")
  class(board) <- c("pins_board_connect", "pins_board")

  out <- rsc_bundle(board, "test", fs::path(path, "test.rds"), metadata, df)

  files <- fs::dir_ls(out)
  files <- files[!fs::is_dir(files)]
  expect_setequal(
    fs::path_file(files),
    c("data.txt", "dictionary.js", "index.html", "manifest.json", "pin.css", "pin.js", "theme.js", "test.rds")
  )
})

test_that("generates index files", {
  board <- list(account = "TEST", url = "http://example.com")
  class(board) <- c("pins_board_connect", "pins_board")
  df <- data.frame(x = 1:2, y = 2:1)
  metadata <- list(
    file = "test.csv",
    file_size = 2908,
    pin_hash = "77fee172a9275a62",
    type = "rds",
    title = "test: a pinned 2 x 2 data frame",
    description = "Some simple data to test with",
    urls = c("https://posit.co/", "https://www.tidyverse.org/"),
    created = "20211111T113956Z",
    api_version = "1.0",
    user = list(my_meta = "User defined metadata")
  )
  expect_snapshot_output(cat(rsc_bundle_preview_index(
    board,
    "test",
    df,
    metadata
  )))
})

test_that("generates preview data", {
  # for data frame
  expect_snapshot({
    df <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
    str(rsc_bundle_preview_data(df))
    str(rsc_bundle_preview_data(df, preview = FALSE))
  })

  # for NULL
  expect_snapshot({
    str(rsc_bundle_preview_data(NULL))
  })
})


# santise -----------------------------------------------------------------

test_that("rsc_bundle_preview_data() sanitisies columns", {
  df <- data.frame(x = as.difftime(1, units = "secs"))
  preview <- rsc_bundle_preview_data(df)
  expect_equal(preview$data$x, "1 secs")
})

test_that("simple types remain as is", {
  expect_equal(sanitise_col(TRUE), TRUE)
  expect_equal(sanitise_col(1.5), 1.5)
  expect_equal(sanitise_col("x"), "x")

  expect_equal(sanitise_col(factor("a")), factor("a"))
  expect_equal(sanitise_col(as.Date("2010-01-01")), as.Date("2010-01-01"))
  expect_equal(sanitise_col(as.POSIXct("2010-01-01")), as.POSIXct("2010-01-01"))
})

test_that("other base types convert with format", {
  expect_equal(sanitise_col(as.difftime(1, units = "secs")), "1 secs")
})

test_that("unless their format method isn't designed for columns", {
  df <- data.frame(x = 1:2)
  expect_equal(sanitise_col(df), rep("No preview available", 2))
  expect_equal(sanitise_col(mean), "No preview available")
})


test_that("the preview keeps pin information and escapes content", {
  local_mocked_bindings(data_dict_cli = function() stop("Unexpected CLI lookup"))
  attack <- '</script><script>alert("unexpected")</script>'
  board <- structure(
    list(account = "TEST", url = "https://connect.example.com"),
    class = c("pins_board_connect", "pins_board")
  )
  metadata <- list(
    file = 'data "<tag>.csv', type = "csv", title = "Sample pin",
    description = attack, created = "20211111T113956Z",
    urls = c("https://example.com/", "javascript:alert(1)"),
    user = list(data_dict_version = "ordinary metadata")
  )
  frame <- data.frame(value = attack)
  output <- withr::local_tempdir()
  rsc_bundle_preview_create(board, "data", frame, metadata, output, TRUE)
  html <- paste(readLines(file.path(output, "index.html")), collapse = "\n")
  expect_false(grepl(attack, html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_match(html, 'href="#metadata"', fixed = TRUE)
  sections <- strsplit(html, '<section id="metadata"', fixed = TRUE)[[1L]]
  expect_false(grepl("Download metadata", sections[[1L]], fixed = TRUE))
  expect_match(sections[[2L]], "Download metadata", fixed = TRUE)
  expect_false(grepl("<details", html, fixed = TRUE))
  expect_match(html, "Sample pin", fixed = TRUE)
  expect_match(html, "data%20%22%3Ctag%3E.csv", fixed = TRUE)
  expect_match(html, "board.pin_read(&quot;TEST/data&quot;)", fixed = TRUE)
  expect_match(html, "https://example.com/", fixed = TRUE)
  expect_false(grepl('href="javascript:', html, fixed = TRUE))
  match <- regmatches(html, regexec(
    '(?s)<script data-pagedtable-source type="application/json">(.*?)</script>',
    html, perl = TRUE
  ))[[1L]]
  expect_equal(jsonlite::fromJSON(match[[2L]])$data$value, attack)
  expect_false(file.exists(file.path(output, "dictionary.html")))
  expect_true(file.exists(file.path(output, "pin.css")))
  expect_true(file.exists(file.path(output, "pin.js")))
})

test_that("preview generation rejects payload filename collisions", {
  board <- structure(list(account = "TEST"), class = c("pins_board_connect", "pins_board"))
  output <- withr::local_tempdir()
  for (file in c("index.html", "INDEX.HTML", "pin.css", "data.txt", "manifest.json", "pagedtable-1.1", "dictionary.html")) {
    metadata <- list(file = file, type = "file")
    if (file == "dictionary.html") {
      metadata$user <- list(data_dict = "tables: [{name: data, columns: []}]")
    }
    expect_error(
      rsc_bundle_preview_create(board, "data", NULL, metadata, output, FALSE),
      "would overwrite"
    )
  }
})


test_that("render failures leave remote content unchanged", {
  calls <- character()
  local_mocked_bindings(
    data_dict_render = function(...) stop("Render failed"),
    rsc_content_find = function(...) { calls <<- c(calls, "find"); list(guid = "existing") },
    rsc_content_update = function(...) { calls <<- c(calls, "update") },
    rsc_content_create = function(...) { calls <<- c(calls, "create"); list(guid = "new") }
  )
  board <- structure(list(account = "TEST", versioned = TRUE),
    class = c("pins_board_connect", "pins_board"))
  payload <- withr::local_tempfile(fileext = ".csv")
  writeLines("id\n1", payload)
  metadata <- list(file = basename(payload), type = "csv",
    user = list(data_dict = "tables: [{name: data, columns: []}]"))
  expect_error(pin_store(board, "data", payload, metadata), "Render failed")
  expect_length(calls, 0L)
})

test_that("failed asset copies stop preview creation", {
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")
  board <- structure(list(account = "TEST"), class = c("pins_board_connect", "pins_board"))
  output <- withr::local_tempdir()
  expect_error(rsc_bundle_preview_create(board, "data", NULL,
    list(file = "data.csv", type = "csv"), output, FALSE), "Could not copy")
  expect_false(file.exists(file.path(output, "index.html")))
})
