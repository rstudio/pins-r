rsconnect_bundle_template_html <- function(temp_dir, template, value) {
  html_file <- file.path(temp_dir, "index.html")
  html_index <- readLines(html_file)
  html_index <- gsub(paste0("\\{\\{", template, "\\}\\}"), value, html_index)
  writeLines(html_index, html_file)
}

rsconnect_bundle_create.data.frame <- function(x, temp_dir) {
  rds_file <- file.path(temp_dir, "data.rds")
  csv_file <- file.path(temp_dir, "data.csv")

  saveRDS(x, rds_file, version = 2)

  file.copy(
    dir(system.file("views/data", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  max_rows <- min(nrow(x), getOption("pins.preview.rows", 10^4))
  data_preview <- list(
    columns = lapply(colnames(x), function(e) {
      list(
        align = "right",
        label = e,
        name = e,
        type = ""
      )
    }),
    data = head(x, n = max_rows),
    options = list(
      columns = list( max = 10 ),
      rows = list ( min = 1, total = nrow(x))
    )
  )

  rsconnect_bundle_template_html(temp_dir, "data_preview", jsonlite::toJSON(data_preview))

  "data.rds"
}

rsconnect_bundle_create.default <- function(x, temp_dir) {
  html_file <- file.path(temp_dir, "index.html")

  file.copy(
    dir(system.file("views/files", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  saveRDS(x, file.path(temp_dir, "data.rds"), version = 2)

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  rsconnect_bundle_template_html(temp_dir, "file_name", paste(files, collapse = "\n"))

  "data.rds"
}

rsconnect_bundle_create.character <- function(x, temp_dir) {
  file.copy(x, temp_dir, recursive = TRUE)

  data_files <- dir(temp_dir, recursive = TRUE)

  html_file <- file.path(temp_dir, "index.html")

  file.copy(
    dir(system.file("views/files", package = "pins"), full.names = TRUE),
    temp_dir,
    recursive = TRUE)

  files <- dir(temp_dir, recursive = TRUE)
  files <- files[!grepl("index\\.html", files)]

  rsconnect_bundle_template_html(temp_dir, "file_name", files)

  data_files
}

rsconnect_bundle_create <- function(x, temp_dir) {
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
