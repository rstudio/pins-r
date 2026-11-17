dictionary <- paste0(
  '$version: 0.1.0\ntables:\n  - name: data\n    columns:\n',
  '      - name: id\n        type: integer\n'
)

data_dict_cli_or_skip <- function() {
  binary <- tryCatch(data_dict_cli(), error = function(error) "")
  if (nzchar(binary)) {
    return(binary)
  }
  if (identical(Sys.getenv("PINS_REQUIRE_DATA_DICT_CLI"), "1")) {
    testthat::fail("PINS_REQUIRE_DATA_DICT_CLI=1 but the data-dict CLI is unavailable.")
  }
  testthat::skip("Optional data-dict CLI not installed")
}

test_that("authored metadata roundtrips without a CLI", {
  local_mocked_bindings(data_dict_cli = function() stop("Unexpected CLI lookup"))
  path <- withr::local_tempfile(fileext = ".yaml")
  writeBin(charToRaw(dictionary), path)
  expect_equal(data_dict_metadata(NULL, path, NULL)[["data_dict"]], dictionary)
  metadata <- list(owner = "example", data_dict_version = "v1")
  ui_loud()
  board <- board_temp()
  frame <- data.frame(id = 1:2)
  pin_write(board, frame, "data", type = "csv", metadata = metadata, data_dict = path)
  expect_equal(metadata, list(owner = "example", data_dict_version = "v1"))
  expect_equal(pin_meta(board, "data")$user, list(
    owner = "example",
    data_dict_version = "v1",
    data_dict = sub("\n$", "", dictionary)
  ))
  pin_write(board, frame, "ordinary-version", type = "csv", metadata = list(data_dict_version = "v1"))
  expect_equal(pin_meta(board, "ordinary-version")$user, list(data_dict_version = "v1"))
  expect_equal(pin_read(board, "data"), frame)
  expect_message(
    pin_write(board, frame, "data", type = "csv", metadata = list(data_dict = paste0(dictionary, "# edit\n"))),
    "force_identical_write = TRUE"
  )
  expect_equal(pin_meta(board, "data")$user[["data_dict"]], sub("\n$", "", dictionary))
})

test_that("metadata-only dictionary updates can be forced", {
  version <- 0L
  local_mocked_bindings(
    data_dict_cli = function() stop("Unexpected CLI lookup"),
    version_name = function(metadata) {
      version <<- version + 1L
      c("20130104T050607Z-xxxxx", "20130204T050607Z-yyyyy")[[version]]
    }
  )
  ui_loud()
  board <- board_temp()
  frame <- data.frame(id = 1:2)
  pin_write(board, frame, "data", type = "csv", metadata = list(data_dict = dictionary), versioned = FALSE)
  edited <- sub("integer", "string", dictionary)

  expect_message(
    pin_write(board, frame, "data", type = "csv", metadata = list(data_dict = edited), versioned = FALSE),
    "force_identical_write = TRUE"
  )
  expect_equal(pin_meta(board, "data")$user[["data_dict"]], sub("\n$", "", dictionary))
  expect_message(
    pin_write(
      board, frame, "data", type = "csv",
      metadata = list(data_dict = edited), force_identical_write = TRUE, versioned = FALSE
    ),
    "Writing to pin"
  )
  expect_equal(pin_meta(board, "data")$user[["data_dict"]], sub("\n$", "", edited))
})

test_that("invalid options, conflicts and unsafe sources are rejected", {
  expect_error(data_dict_metadata(NULL, FALSE, NULL), "must be NULL, TRUE")
  expect_error(data_dict_metadata(list(data_dict = dictionary), TRUE, NULL), "not both")
  expect_error(data_dict_metadata(list(data_dict = "tables: [oops]"), NULL, NULL), "table mappings")
  expect_error(data_dict_metadata(list(data_dict = "tables: []"), NULL, NULL), "table mappings")
  expect_error(data_dict_metadata(list(data_dict = "tables: {foo: {name: foo}}"), NULL, NULL), "table mappings")
  expect_error(data_dict_metadata(list(data_dict = "[broken"), NULL, NULL), "Invalid data dictionary")
  source <- 'template: &table {name: data, source: {parquet: /secret/data.parquet}}\ntables: [*table]'
  expect_error(data_dict_metadata(list(data_dict = source), NULL, NULL), "omit table source")
  expect_error(data_dict_metadata(NULL, TRUE, list(id = 1)), "data frame")
  expect_error(data_dict_metadata(NULL, TRUE, "data.csv", upload = TRUE), "Parquet")
})

test_that("dictionary file errors name the path", {
  directory <- withr::local_tempdir()
  missing <- file.path(directory, "missing.yaml")
  missing_error <- tryCatch(data_dict_metadata(NULL, missing, NULL), error = identity)
  expect_s3_class(missing_error, "error")
  expect_match(conditionMessage(missing_error), "does not exist or is not a file")
  expect_match(conditionMessage(missing_error), fs::path_expand(missing), fixed = TRUE)

  invalid <- file.path(directory, "invalid-utf8.yaml")
  writeBin(as.raw(c(0xc3, 0x28)), invalid)
  utf8_error <- tryCatch(data_dict_metadata(NULL, invalid, NULL), error = identity)
  expect_s3_class(utf8_error, "error")
  expect_match(conditionMessage(utf8_error), "valid UTF-8")
  expect_match(conditionMessage(utf8_error), fs::path_expand(invalid), fixed = TRUE)
})

test_that("missing CLI and entrypoint collisions fail clearly", {
  local_mocked_bindings(data_dict_cli = function() cli::cli_abort("Missing data-dict CLI"))
  expect_error(data_dict_run("--version"), "Missing data-dict CLI")
  path <- withr::local_tempdir()
  writeLines("publisher's original", file.path(path, "index.html"))
  expect_error(data_dict_render(dictionary, path), "overwrite.*index.html")
  expect_equal(readLines(file.path(path, "index.html")), "publisher's original")
})

test_that("the data-dict CLI is optional", {
  cli <- data_dict_cli
  test_env <- new.env(parent = environment(cli))
  test_env$Sys.getenv <- function(...) ""
  test_env$file.exists <- function(...) FALSE
  test_env$Sys.which <- function(...) ""
  environment(cli) <- test_env

  expect_error(cli(), "Install the optional datadict package")
})

test_that("only the supported data-dict CLI version is accepted", {
  cli <- data_dict_cli
  test_env <- new.env(parent = environment(cli))
  binary <- unname(Sys.which("Rscript"))
  test_env$Sys.getenv <- function(...) binary
  version <- "data-dict 0.0.3"
  test_env$system2 <- function(...) version
  environment(cli) <- test_env

  expect_identical(cli(), binary)
  version <- "data-dict 0.0.4"
  expect_error(cli(), "Unsupported data-dict CLI version.*0.0.4")
  version <- "unrecognized version"
  expect_error(cli(), "Unsupported data-dict CLI version.*unknown")
})

test_that("the CLI version guard rejects suffixed versions", {
  cli <- data_dict_cli
  test_env <- new.env(parent = environment(cli))
  test_env$Sys.getenv <- function(...) unname(Sys.which("Rscript"))
  test_env$system2 <- function(...) "data-dict version 0.0.3.dev1"
  environment(cli) <- test_env

  expect_error(cli(), "Unsupported data-dict CLI version")
})

test_that("a failed CLI version check reports its status and output", {
  cli <- data_dict_cli
  test_env <- new.env(parent = environment(cli))
  test_env$Sys.getenv <- function(...) unname(Sys.which("Rscript"))
  test_env$system2 <- function(...) structure("version check failed", status = 4L)
  environment(cli) <- test_env

  expect_error(cli(), "status 4.*version check failed")
})

test_that("draft removes source paths from every table", {
  draft <- paste(
    "$version: 0.1.0",
    "tables:",
    "  - name: first",
    "    source: {parquet: /private/first.parquet}",
    "    columns:",
    "      - {name: id, type: integer}",
    "  - name: second",
    "    source: {parquet: /private/second.parquet}",
    "    columns:",
    "      - {name: value, type: string}",
    sep = "\n"
  )
  local_mocked_bindings(data_dict_run = function(args) writeLines(draft, args[[4L]]))

  metadata <- data_dict_metadata(NULL, TRUE, data.frame(id = 1:2))
  tables <- yaml::yaml.load(metadata[["data_dict"]])[["tables"]]
  expect_length(tables, 2L)
  expect_true(all(vapply(tables, function(table) is.null(table[["source"]]), logical(1))))
})

test_that("render adds a source only for one Parquet file and one table", {
  rendered <- NULL
  local_mocked_bindings(data_dict_run = function(args) {
    rendered <<- yaml::read_yaml(args[[2L]])
    writeLines("<html><head></head><body>rendered</body></html>", args[[4L]])
  })

  uppercase <- withr::local_tempdir()
  output <- data_dict_render(dictionary, uppercase, "data.PARQUET", filename = "dictionary.html")
  expect_equal(output, file.path(uppercase, "dictionary.html"))
  expect_equal(rendered[["tables"]][[1L]][["source"]][["parquet"]], "data.PARQUET")

  multiple_files <- withr::local_tempdir()
  data_dict_render(dictionary, multiple_files, c("first.parquet", "second.parquet"))
  expect_false("source" %in% names(rendered[["tables"]][[1L]]))

  multiple_tables <- paste0(
    dictionary,
    "  - name: other\n    columns:\n      - name: value\n        type: string\n"
  )
  multiple_tables_dir <- withr::local_tempdir()
  data_dict_render(multiple_tables, multiple_tables_dir, "data.parquet")
  expect_true(all(vapply(rendered[["tables"]], function(table) {
    !"source" %in% names(table)
  }, logical(1))))
})

test_that("CLI failures report their exit status and output", {
  binary <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  local_mocked_bindings(data_dict_cli = function() binary)
  expect_error(
    data_dict_run(c("-e", "message('invalid dictionary'); quit(status = 7)")),
    "data-dict failed \\(status 7\\): invalid dictionary"
  )
})

test_that("real CLI drafts portable YAML and renders the Connect entrypoint", {
  data_dict_cli_or_skip()
  board <- board_temp()
  frame <- data.frame(id = 1:2, value = c(10, 20))
  pin_write(board, frame, "data", type = "csv", data_dict = TRUE)
  metadata <- pin_meta(board, "data")
  document <- yaml::yaml.load(metadata$user[["data_dict"]])
  expect_equal(vapply(document[["tables"]][[1L]][["columns"]], `[[`, character(1), "name"), c("id", "value"))
  expect_false("source" %in% names(document[["tables"]][[1L]]))
  expect_equal(pin_read(board, "data"), frame)
  paths <- pin_download(board, "data")
  metadata$local <- metadata$version <- NULL
  metadata$created <- as_8601_compact(metadata$created)
  connect <- structure(list(account = "TEST", url = "https://example.com"),
                       class = c("pins_board_connect", "pins_board"))
  output <- withr::local_tempdir()
  rsc_bundle(connect, "data", paths, metadata, frame, bundle_path = output)
  index <- paste(readLines(file.path(output, "index.html")), collapse = "\n")
  dictionary_html <- paste(readLines(file.path(output, "dictionary.html")), collapse = "\n")
  expect_match(index, "dictionary.html")
  expect_match(dictionary_html, "Data dictionary")
  expect_match(dictionary_html, '<script src="theme.js"></script>', fixed = TRUE)
  expect_match(dictionary_html, "#theme-toggle{display:none}", fixed = TRUE)
  expect_equal(yaml::read_yaml(file.path(output, "data.txt"))$user, metadata$user)
  expect_false(is.na(read_meta(output)$created))
  expect_equal(utils::read.csv(file.path(output, basename(paths))), frame)
  expect_equal(jsonlite::read_json(file.path(output, "manifest.json"))$metadata$primary_html, "index.html")
  parquet <- withr::local_tempfile(fileext = ".parquet")
  write_parquet(frame, parquet)
  pin_upload(board, parquet, "upload", data_dict = TRUE)
  uploaded <- yaml::yaml.load(pin_meta(board, "upload")$user[["data_dict"]])
  expect_false("source" %in% names(uploaded[["tables"]][[1L]]))
  failed <- withr::local_tempdir()
  expect_error(data_dict_render(sub("integer", "not-a-data-dict-type", dictionary), failed), "data-dict failed")
  expect_false(file.exists(file.path(failed, "index.html")))
})

test_that("Connect Parquet rendering embeds profiles without changing pin metadata", {
  data_dict_cli_or_skip()
  frame <- data.frame(id = 1:3, value = c(10, NA_real_, 30), group = c("a", "b", "a"))
  board <- board_temp()
  authored <- withr::local_tempfile(fileext = ".yaml")
  writeLines(c(
    "$version: 0.1.0", "tables:", "  - name: data", "    columns:",
    "      - {name: id, type: 'number(id)', examples: [1], constraints: [primary_key]}",
    "      - {name: value, type: number, examples: [10]}",
    "      - {name: group, type: string, examples: [a]}"
  ), authored)
  pin_write(board, frame, "data", type = "parquet", data_dict = authored)
  metadata <- pin_meta(board, "data")
  metadata$local <- metadata$version <- NULL
  metadata$created <- as_8601_compact(metadata$created)
  paths <- pin_download(board, "data")
  connect <- structure(list(account = "TEST", url = "https://example.com"),
                       class = c("pins_board_connect", "pins_board"))
  output <- withr::local_tempdir()
  rsc_bundle(connect, "data", paths, metadata, frame, bundle_path = output)
  html <- paste(readLines(file.path(output, "dictionary.html")), collapse = "\n")
  match <- regmatches(html, regexec('(?s)<script[^>]*id="dict"[^>]*>(.*?)</script>', html, perl = TRUE))[[1L]]
  table <- jsonlite::fromJSON(match[[2L]], simplifyVector = FALSE)$tables[[1L]]
  profiles <- setNames(lapply(table$columns, `[[`, "profile"), vapply(table$columns, `[[`, character(1), "name"))
  expect_equal(table$rows, 3)
  expect_equal(table$source$parquet, as.character(metadata$file))
  expect_equal(profiles$id$distinct$count, 3)
  expect_equal(table$columns[[1L]]$examples, list(1))
  expect_true("primary_key" %in% unlist(table$columns[[1L]]$constraints))
  expect_equal(profiles$value$missing, 1)
  expect_equal(profiles$value$range, list(min = 10, max = 30))
  expect_equal(sum(vapply(profiles$value$histogram$bins, `[[`, numeric(1), "count")), 2)
  expect_equal(profiles$group$common_values$values, list(list(value = "a", count = 2), list(value = "b", count = 1)))
  expect_equal(yaml::read_yaml(file.path(output, "data.txt"))$user, metadata$user)
  expect_false("source" %in% names(yaml::yaml.load(metadata$user[["data_dict"]])[["tables"]][[1L]]))
  expect_setequal(list.files(output), c(
    "index.html", "dictionary.html", "dictionary.js", "data.txt", "manifest.json",
    "highlight.js-9.15.9", "pagedtable-1.1", "pin.css", "pin.js", "theme.js",
    as.character(metadata$file)
  ))
  expect_false(grepl(output, html, fixed = TRUE))
  expect_equal(as.data.frame(nanoparquet::read_parquet(file.path(output, metadata$file))), frame)
})

test_that("failed renders remove partial HTML and temporary YAML", {
  local_mocked_bindings(data_dict_run = function(args) {
    writeLines("partial output", args[[4L]])
    stop("Render failed")
  })
  output <- withr::local_tempdir()
  expect_error(data_dict_render(dictionary, output, filename = "dictionary.html"), "Render failed")
  expect_length(list.files(output), 0L)
})

test_that("drafting removes sources from every table and resolves upload paths", {
  arguments <- NULL
  local_mocked_bindings(data_dict_run = function(args) {
    arguments <<- args
    writeLines(c("tables:",
      "  - {name: first, source: {parquet: first.parquet}, columns: []}",
      "  - {name: second, source: {parquet: second.parquet}, columns: []}"),
      args[[length(args)]])
  })
  output <- data_dict_metadata(NULL, TRUE, c("first.parquet", "second.parquet"), upload = TRUE)
  document <- yaml::yaml.load(output[["data_dict"]])
  expect_equal(length(document[["tables"]]), 2L)
  expect_true(all(vapply(document[["tables"]], function(table) is.null(table[["source"]]), logical(1))))
  expect_equal(arguments[2:3], as.character(fs::path_abs(c("first.parquet", "second.parquet"))))
})

test_that("render preserves a pre-existing dangling output symlink", {
  skip_on_os("windows")
  output <- withr::local_tempdir()
  html <- file.path(output, "dictionary.html")
  fs::link_create(file.path(output, "missing.html"), html)
  expect_error(data_dict_render(dictionary, output, filename = "dictionary.html"), "overwrite")
  expect_true(fs::link_exists(html))
})
