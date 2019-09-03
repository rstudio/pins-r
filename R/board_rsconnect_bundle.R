rsconnect_bundle_template_html <- function(temp_dir, template, value) {
  html_file <- file.path(temp_dir, "index.html")
  html_index <- readLines(html_file)
  html_index <- gsub(paste0("\\{\\{", template, "\\}\\}"), value, html_index)
  writeLines(html_index, html_file)
}

rsconnect_bundle_files_html <- function(files) {
  html <- ""
  for (file in files) {
    html <- paste0(html, "<a href=\"", file, "\">", file, "</a> ")
  }

  html
}

rsconnect_bundle_template_common <- function(temp_dir, style, name, board, account_name) {
  rsconnect_bundle_template_html(temp_dir, "data_preview_style", style)
  rsconnect_bundle_template_html(temp_dir, "pin_name", name)
  if (is.character(board$server))
    rsconnect_bundle_template_html(temp_dir, "server_name", board$server)
  else
    rsconnect_bundle_template_html(temp_dir, "server_name", "https://rstudio-connect-server")
  rsconnect_bundle_template_html(temp_dir, "account_name", account_name)
}

rsconnect_bundle_create.data.frame <- function(x, temp_dir, name, board, account_name) {
  file.copy(
    dir(system.file("views/data", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  max_rows <- min(nrow(x), getOption("pins.preview.rows", 10^4))

  csv_name <- dir(temp_dir, "data\\.csv")

  x_preview <- utils::head(x, n = max_rows)
  x_preview <- data.frame(lapply(x_preview, function(e) {
    if (!is.numeric(e) || !is.integer(e) || !is.logical(e) || !is.double(e)) {
      char_column <- as.character(e)
      if (length(char_column) == nrow(x_preview))
        char_column
      else
        rep("...", nrow(x_preview))
    } else
      e
  }), stringsAsFactors = FALSE)

  data_preview <- list(
    columns = lapply(colnames(x_preview), function(e) {
      list(
        align = "right",
        label = e,
        name = e,
        type = ""
      )
    }),
    data = x_preview,
    options = list(
      columns = list( max = 10 ),
      rows = list (min = 1, total = nrow(x_preview))
    )
  )

  rsconnect_bundle_template_html(temp_dir, "files_html", rsconnect_bundle_files_html(csv_name))
  rsconnect_bundle_template_html(temp_dir, "data_preview", jsonlite::toJSON(data_preview))
  rsconnect_bundle_template_common(temp_dir, "", name, board, account_name)

  "data.rds"
}

rsconnect_bundle_create.default <- function(x, temp_dir, name, board, account_name) {
  html_file <- file.path(temp_dir, "index.html")

  saveRDS(x, file.path(temp_dir, "data.rds"), version = 2)

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  file.copy(
    dir(system.file("views/data", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  rsconnect_bundle_template_html(temp_dir, "files_html", rsconnect_bundle_files_html(files))
  rsconnect_bundle_template_html(temp_dir, "data_preview", "{\"data\": [], \"columns\": []}")
  rsconnect_bundle_template_common(temp_dir, "display: none", name, board, account_name)

  "data.rds"
}

rsconnect_bundle_create.character <- function(x, temp_dir, name, board, account_name) {
  file.copy(dir(x, full.names = TRUE), temp_dir, recursive = TRUE)

  data_files <- dir(temp_dir, recursive = TRUE)

  html_file <- file.path(temp_dir, "index.html")

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  file.copy(
    dir(system.file("views/data", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  rsconnect_bundle_template_html(temp_dir, "files_html", rsconnect_bundle_files_html(files))
  rsconnect_bundle_template_html(temp_dir, "data_preview", "{\"data\": [], \"columns\": []}")
  rsconnect_bundle_template_common(temp_dir, "display: none", name, board, account_name)

  data_files
}

rsconnect_bundle_create <- function(x, temp_dir, name, board, account_name) {
  UseMethod("rsconnect_bundle_create")
}

rsconnect_bundle_compress <- function(path, manifest) {
  manifest_json <- jsonlite::toJSON(manifest,
                                    dataframe = "columns",
                                    null = "null",
                                    na = "null",
                                    auto_unbox = TRUE,
                                    pretty = TRUE)
  writeLines(manifest_json, file.path(path, "manifest.json"), useBytes = TRUE)

  prev_path <- setwd(path)
  on.exit(setwd(prev_path), add = TRUE)

  bundle_path <- tempfile("rsconnect-bundle", fileext = ".tar.gz")
  utils::tar(bundle_path, files = ".", compression = "gzip", tar = "internal")

  bundle_path
}

rsconnect_bundle_file_md5 <- function(path) {
  con <- base::file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  unclass(as.character(openssl::md5(con)))
}
