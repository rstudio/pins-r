data_dict_cli <- function() {
  binary <- ""
  if (requireNamespace("datadict", quietly = TRUE)) {
    binary <- datadict::dd_path(check = FALSE)
  }
  if (!nzchar(binary)) {
    binary <- unname(Sys.which("data-dict"))
  }
  if (!nzchar(binary)) {
    cli::cli_abort(
      "Install the optional datadict package and run datadict::dd_install(), or put data-dict on PATH."
    )
  }
  binary
}

data_dict_run <- function(args) {
  binary <- data_dict_cli()
  output <- tempfile()
  withr::defer(unlink(output))
  status <- tryCatch(
    suppressWarnings(system2(
      binary, shQuote(args), stdout = output, stderr = output,
      timeout = 300
    )),
    error = function(error) cli::cli_abort(
      "Could not run data-dict: {conditionMessage(error)}", parent = error
    )
  )
  if (status != 0L) {
    message <- paste(readLines(output, warn = FALSE), collapse = "\n")
    cli::cli_abort("data-dict failed (status {status}): {message}")
  }
  invisible(NULL)
}

data_dict_parse <- function(text, allow_source = FALSE) {
  if (!is_string(text)) {
    cli::cli_abort("metadata$data_dict must contain YAML text.")
  }
  document <- tryCatch(
    yaml::yaml.load(text, eval.expr = FALSE, handlers = list(seq = as.list)),
    error = function(error) cli::cli_abort(
      "Invalid data dictionary YAML: {conditionMessage(error)}", parent = error
    )
  )
  tables <- if (is.list(document)) document[["tables"]] else NULL
  if (!is.list(tables) || !is.null(names(tables)) ||
      length(tables) == 0L ||
      !all(vapply(tables, function(table) {
        is.list(table) && !is.null(names(table))
      }, logical(1)))) {
    cli::cli_abort("A data dictionary must contain a list of table mappings.")
  }
  if (!allow_source && any(vapply(tables, function(table) {
    "source" %in% names(table)
  }, logical(1)))) {
    cli::cli_abort("Pin data dictionaries must omit table source fields.")
  }
  document
}

data_dict_draft <- function(x, upload) {
  directory <- tempfile()
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  output <- file.path(directory, "data-dict.yaml")
  if (upload) {
    paths <- fs::path_abs(x)
    if (!length(paths) || any(tolower(fs::path_ext(paths)) != "parquet")) {
      cli::cli_abort("data_dict = TRUE for pin_upload() requires Parquet files.")
    }
  } else {
    if (!is.data.frame(x)) {
      cli::cli_abort("data_dict = TRUE for pin_write() requires a data frame.")
    }
    paths <- file.path(directory, "data.parquet")
    write_parquet(x, paths)
  }
  data_dict_run(c("draft", paths, "-o", output))
  document <- data_dict_parse(
    paste(readLines(output), collapse = "\n"), allow_source = TRUE
  )
  tables <- document[["tables"]]
  document[["tables"]] <- lapply(tables, function(table) {
    table[["source"]] <- NULL
    table
  })
  yaml::as.yaml(document)
}

data_dict_metadata <- function(metadata, data_dict, x, upload = FALSE) {
  dictionary <- metadata[["data_dict"]]
  if (is.null(data_dict) && is.null(dictionary)) {
    return(metadata)
  }
  if (!is.null(data_dict) && !is.null(dictionary)) {
    cli::cli_abort("Supply data_dict or metadata$data_dict, not both.")
  }
  if (isTRUE(data_dict)) {
    metadata[["data_dict"]] <- data_dict_draft(x, upload)
  } else if (is_string(data_dict)) {
    path <- fs::path_expand(data_dict)
    if (!fs::is_file(path)) {
      cli::cli_abort(
        "Data dictionary file {.file {path}} does not exist or is not a file."
      )
    }
    bytes <- tryCatch(
      readBin(path, "raw", n = fs::file_size(path)),
      error = function(error) cli::cli_abort(
        "Could not read data dictionary file {.file {path}}: {conditionMessage(error)}",
        parent = error
      )
    )
    text <- tryCatch(
      rawToChar(bytes),
      error = function(error) cli::cli_abort(
        "Could not read data dictionary file {.file {path}}: {conditionMessage(error)}",
        parent = error
      )
    )
    if (!validUTF8(text)) {
      cli::cli_abort("Data dictionary file {.file {path}} must contain valid UTF-8.")
    }
    Encoding(text) <- "UTF-8"
    metadata[["data_dict"]] <- text
  } else if (!is.null(data_dict)) {
    cli::cli_abort("data_dict must be NULL, TRUE, or a YAML file path.")
  }
  data_dict_parse(metadata[["data_dict"]])
  metadata
}

data_dict_render <- function(
  text, directory, files = character(), filename = "index.html"
) {
  document <- data_dict_parse(text)
  html_path <- file.path(directory, filename)
  if (file.exists(html_path) || fs::link_exists(html_path)) {
    cli::cli_abort("Generating a data dictionary would overwrite {.file {filename}}.")
  }
  # Profiles use the bundle-relative path for one Parquet file and one table.
  if (length(files) == 1L && tolower(fs::path_ext(files)) == "parquet" &&
      length(document[["tables"]]) == 1L) {
    tables <- document[["tables"]]
    tables[[1L]][["source"]] <- list(parquet = files[[1L]])
    document[["tables"]] <- tables
    text <- yaml::as.yaml(document)
  }
  dictionary <- tempfile(tmpdir = directory, fileext = ".yaml")
  withr::defer(unlink(dictionary))
  writeBin(charToRaw(enc2utf8(text)), dictionary)
  complete <- FALSE
  withr::defer(if (!complete) unlink(html_path))
  data_dict_run(c("render", dictionary, "-o", html_path))
  if (!file.exists(html_path)) {
    cli::cli_abort("data-dict did not produce {.file {filename}}.")
  }
  if (filename == "dictionary.html") {
    html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    integration <- '<script src="theme.js"></script><style>#theme-toggle{display:none}</style>'
    html <- sub("</head>", paste0(integration, "</head>"), html, fixed = TRUE)
    writeBin(charToRaw(enc2utf8(paste0(html, "\n"))), html_path)
  }
  complete <- TRUE
  invisible(html_path)
}
