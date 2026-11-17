rsc_bundle <- function(
  board,
  name,
  paths,
  metadata,
  x = NULL,
  bundle_path = tempfile(),
  preview_data = TRUE
) {
  rsc_bundle_check_files(metadata)
  fs::dir_create(bundle_path)

  # Bundle contains:
  # * pin
  fs::file_copy(paths, fs::path(bundle_path, fs::path_file(paths)))

  # * data.txt (used to retrieve pins)
  write_yaml(metadata, fs::path(bundle_path, "data.txt"))

  # * index.html
  rsc_bundle_preview_create(board, name, x, metadata, bundle_path, preview_data)

  # * manifest.json (used for deployment)
  manifest <- rsc_bundle_manifest(board, bundle_path)
  jsonlite::write_json(
    manifest,
    fs::path(bundle_path, "manifest.json"),
    auto_unbox = TRUE
  )

  invisible(bundle_path)
}

# Extracted from rsconnect:::createAppManifest
rsc_bundle_manifest <- function(board, path) {
  files <- fs::path_rel(fs::dir_ls(path, recurse = TRUE, type = "file"), path)

  list(
    version = 1,
    locale = "en_US",
    platform = "3.5.1",
    metadata = list(
      appmode = "static",
      primary_rmd = NA,
      primary_html = "index.html",
      content_category = "pin",
      has_parameters = FALSE
    ),
    packages = NA,
    files = files,
    users = NA
  )
}

rsc_bundle_check_files <- function(metadata) {
  template_path <- fs::path_package("pins", "preview")
  reserved <- c("data.txt", "manifest.json", as.character(fs::path_rel(
    fs::dir_ls(template_path, recurse = TRUE), template_path
  )))
  if (!is.null(metadata[["user"]][["data_dict"]])) {
    reserved <- c(reserved, "dictionary.html")
  }
  overlap <- metadata[["file"]][tolower(metadata[["file"]]) %in% tolower(reserved)]
  if (length(overlap)) {
    cli::cli_abort("Generating a pin preview would overwrite: {paste(overlap, collapse = ', ')}")
  }
  invisible(NULL)
}

rsc_bundle_preview_create <- function(
  board,
  name,
  x,
  metadata,
  path,
  preview_data
) {
  rsc_bundle_check_files(metadata)
  dictionary <- metadata[["user"]][["data_dict"]]
  template_path <- fs::path_package("pins", "preview")
  if (!is.null(dictionary)) {
    data_dict_render(dictionary, path, metadata[["file"]], filename = "dictionary.html")
  }
  template <- fs::dir_ls(template_path)
  if (!all(file.copy(template, path, recursive = TRUE))) {
    cli::cli_abort("Could not copy pin preview assets.")
  }

  # Update index template with pin data
  index <- rsc_bundle_preview_index(board, name, x, metadata, preview_data)
  writeLines(index, fs::path(path, "index.html"))

  invisible(path)
}

rsc_bundle_preview_index <- function(
  board,
  name,
  x,
  metadata,
  preview_data = TRUE
) {
  data_preview <- rsc_bundle_preview_data(x, preview_data)
  if (is.data.frame(x) && preview_data) {
    data_preview$options$rows$min <- max(1L, min(10L, nrow(x)))
    data_preview$options$rows$max <- 10L
  }
  name <- rsc_parse_name(name)
  owner <- name$owner %||% board$account

  pin_name <- paste0(owner, "/", name$name)
  urls <- metadata[["urls"]]
  urls <- urls[grepl("^https?://", urls)]
  data <- list(
    pin_files = lapply(metadata[["file"]], function(file) {
      list(name = file, url = utils::URLencode(file, reserved = TRUE))
    }),
    data_preview = gsub(
      "<", "\\u003c", jsonlite::toJSON(data_preview, auto_unbox = TRUE), fixed = TRUE
    ),
    has_preview = is.data.frame(x) && preview_data,
    has_dictionary = !is.null(metadata[["user"]][["data_dict"]]),
    urls = lapply(urls, function(url) list(url = url)),
    has_urls = length(urls) > 0L,
    show_python = !all(metadata$type %in% c("rds", "qs2")),
    read_method = if (identical(metadata$type, "file")) "pin_download" else "pin_read",
    pin_name = pin_name,
    pin_name_literal = as.character(jsonlite::toJSON(pin_name, auto_unbox = TRUE)),
    pin_metadata = list(
      as_yaml = yaml::as.yaml(metadata),
      title = metadata$title,
      date = if (is.null(metadata$created)) NULL else {
        format(parse_8601_compact(metadata$created), tz = "UTC")
      },
      format = paste(metadata$type, collapse = ", "),
      api_version = metadata$api_version %||% "0",
      hash = metadata$pin_hash,
      description = metadata$description
    ),
    board_deparse = paste0(expr_deparse(board_deparse(board)), collapse = "\n")
  )

  template <- readLines(fs::path_package("pins", "preview", "index.html"))
  whisker::whisker.render(template, data)
}

rsc_bundle_preview_data <- function(df, preview = TRUE, n = 100) {
  if (!is.data.frame(df) || !preview) {
    return(list(data = list(), columns = list()))
  }

  cols <- lapply(colnames(df), function(col) {
    list(
      name = col,
      label = col,
      align = if (is.numeric(df[[col]])) "right" else "left",
      type = ""
    )
  })

  rows <- utils::head(df, n = n)
  rows[] <- lapply(rows, sanitise_col)

  # https://github.com/mlverse/pagedtablejs
  list(
    data = rows,
    columns = cols,
    options = list(
      columns = list(max = 10),
      rows = list(min = 1, total = nrow(rows))
    )
  )
}

sanitise_col <- function(x) {
  # Basic classes can be left as is
  if (
    is_bare_atomic(x) ||
      is.factor(x) ||
      inherits(x, "Date") ||
      inherits(x, "POSIXt")
  ) {
    return(x)
  }

  # Otherwise attempt to convert to a string, making trying to protect against
  # columns where the format() method doesn't adhere to my expectation
  fmt <- format(x, digits = 3)

  if (is.character(fmt) && length(fmt) == NROW(x)) {
    fmt
  } else {
    rep("No preview available", NROW(x))
  }
}
