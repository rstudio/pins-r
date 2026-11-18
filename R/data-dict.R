data_dict_check_cli <- function() {
  check_installed("datadict", reason = "to draft or render a pin data dictionary")
  output <- datadict::dd_run("--version", timeout = 5)$output
  if (!"data-dict 0.0.3" %in% output) {
    cli::cli_abort(c(
      "Unsupported data-dict CLI version: {.val {paste(output, collapse = ' ')}}.",
      "i" = "Install the supported version with {.code datadict::dd_install(version = '0.0.3')}."
    ))
  }
}

data_dict_run <- function(args) {
  data_dict_check_cli()
  datadict::dd_run(args, timeout = 300)
}

data_dict_parse <- function(text, allow_source = FALSE, call = caller_env()) {
  if (!is_string(text)) {
    cli::cli_abort("metadata$data_dict must contain YAML text.", call = call)
  }
  document <- tryCatch(
    yaml::yaml.load(text, eval.expr = FALSE, handlers = list(seq = as.list)),
    error = function(error) cli::cli_abort(
      "Invalid data dictionary YAML: {conditionMessage(error)}", parent = error, call = call
    )
  )
  tables <- if (is.list(document)) document[["tables"]] else NULL
  if (!is.list(tables) || !is.null(names(tables)) ||
      length(tables) == 0L ||
      !all(vapply(tables, function(table) {
        is.list(table) && !is.null(names(table))
      }, logical(1)))) {
    cli::cli_abort("A data dictionary must contain a list of table mappings.", call = call)
  }
  if (!allow_source && any(vapply(tables, function(table) {
    "source" %in% names(table)
  }, logical(1)))) {
    cli::cli_abort("Pin data dictionaries must omit table source fields.", call = call)
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

data_dict_metadata <- function(metadata, data_dict, x, upload = FALSE, call = caller_env()) {
  dictionary <- metadata[["data_dict"]]
  if (is.null(data_dict) && is.null(dictionary)) {
    return(metadata)
  }
  if (!is.null(data_dict) && !is.null(dictionary)) {
    cli::cli_abort("Supply data_dict or metadata$data_dict, not both.", call = call)
  }
  if (isTRUE(data_dict)) {
    metadata[["data_dict"]] <- data_dict_draft(x, upload)
  } else if (is_string(data_dict)) {
    path <- fs::path_expand(data_dict)
    if (!fs::is_file(path)) {
      cli::cli_abort(
        "Data dictionary file {.file {path}} does not exist or is not a file.",
        call = call
      )
    }
    text <- tryCatch(
      rawToChar(readBin(path, "raw", n = fs::file_size(path))),
      error = function(error) cli::cli_abort(
        "Could not read data dictionary file {.file {path}}: {conditionMessage(error)}",
        parent = error, call = call
      )
    )
    if (!validUTF8(text)) {
      cli::cli_abort("Data dictionary file {.file {path}} must contain valid UTF-8.", call = call)
    }
    Encoding(text) <- "UTF-8"
    metadata[["data_dict"]] <- text
  } else if (!is.null(data_dict)) {
    cli::cli_abort("data_dict must be NULL, TRUE, or a YAML file path.", call = call)
  }
  data_dict_parse(metadata[["data_dict"]], call = call)
  metadata
}

data_dict_secure_html <- function(html, theme = FALSE) {
  head <- regexpr("<head\\b[^>]*>", html, perl = TRUE, ignore.case = TRUE)
  if (head[[1L]] < 0L || !grepl("</head>", html, fixed = TRUE)) {
    cli::cli_abort("data-dict produced HTML without a complete head element.")
  }
  security <- paste(readLines(
    fs::path_package("pins", "preview", "dictionary.js"), encoding = "UTF-8", warn = FALSE
  ), collapse = "\n")
  integration <- paste0("<script>", security, "</script>")
  if (theme) {
    integration <- paste0(integration,
      '<script src="theme.js"></script><style>#theme-toggle{display:none}</style>')
  }
  html <- sub("</head>", paste0(integration, "</head>"), html, fixed = TRUE)
  scripts <- regmatches(html, gregexpr(
    "(?is)<script\\b[^>]*>(.*?)</script\\s*>", html, perl = TRUE
  ))[[1L]]
  scripts <- sub("(?is)^<script\\b[^>]*>", "", scripts, perl = TRUE)
  scripts <- sub("(?is)</script\\s*>$", "", scripts, perl = TRUE)
  hashes <- vapply(scripts, function(script) {
    paste0("'sha256-", jsonlite::base64_enc(digest::digest(
      charToRaw(enc2utf8(script)), algo = "sha256", serialize = FALSE, raw = TRUE
    )), "'")
  }, character(1))
  policy <- paste0("script-src 'self' ", paste(hashes, collapse = " "),
    "; object-src 'none'; base-uri 'none'")
  meta <- paste0('<meta http-equiv="Content-Security-Policy" content="', policy, '">')
  at <- head[[1L]] + attr(head, "match.length") - 1L
  paste0(substr(html, 1L, at), meta, substr(html, at + 1L, nchar(html)))
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
  html <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- data_dict_secure_html(html, theme = filename == "dictionary.html")
  writeBin(charToRaw(enc2utf8(paste0(html, "\n"))), html_path)
  complete <- TRUE
  invisible(html_path)
}
