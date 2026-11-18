# Build a pin bundle from this checkout for deployment with rsconnect-python.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript examples/data-dict/create-bundle.R OUTPUT [DICTIONARY_YAML | --no-dictionary]")
}
pkgload::load_all(".", quiet = TRUE)
output <- args[[1L]]
if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("The output directory must be empty.")
}
dictionary <- if (length(args) == 1L) {
  TRUE
} else if (identical(args[[2L]], "--no-dictionary")) {
  NULL
} else {
  args[[2L]]
}

data <- data.frame(id = 1:3, value = c(10, 20, 30), group = c("a", "b", "a"))
local <- pins::board_temp()
pins::pin_write(
  local, data, name = "dictionary-example", type = "parquet",
  metadata = list(example_language = "R"), data_dict = dictionary
)
stopifnot(isTRUE(all.equal(as.data.frame(pins::pin_read(local, "dictionary-example")), data)))
meta <- pins::pin_meta(local, "dictionary-example")
yaml_text <- meta$user[["data_dict"]]
stopifnot(identical(meta$user[["example_language"]], "R"))
if (!is.null(dictionary)) {
  tables <- yaml::yaml.load(yaml_text)[["tables"]]
  stopifnot(all(vapply(tables, function(table) is.null(table[["source"]]), logical(1))))
} else {
  stopifnot(is.null(yaml_text))
}
paths <- pins::pin_download(local, "dictionary-example")
meta$local <- NULL
meta$version <- NULL
meta$created <- pins:::as_8601_compact(meta$created)

# Prepare the Connect bundle offline; the deployment CLI handles OAuth.
connect <- structure(
  list(
    account = Sys.getenv("CONNECT_ACCOUNT", unset = "example"),
    url = Sys.getenv("CONNECT_SERVER", unset = "https://connect.example.com"),
    server_name = "example"
  ),
  class = c("pins_board_connect", "pins_board")
)
pins:::rsc_bundle(connect, "data-dict-example", paths, meta, x = data, bundle_path = output)
uploaded <- yaml::read_yaml(file.path(output, "data.txt"))
stopifnot(identical(uploaded$user[["data_dict"]], yaml_text))
stopifnot(!is.na(pins:::read_meta(output)$created))
stopifnot(isTRUE(all.equal(as.data.frame(nanoparquet::read_parquet(file.path(output, uploaded$file))), data)))
manifest <- jsonlite::read_json(file.path(output, "manifest.json"))
stopifnot(identical(manifest$metadata$primary_html, "index.html"))
if (!is.null(dictionary)) {
  stopifnot(file.exists(file.path(output, "dictionary.html")))
  stopifnot(any(grepl("dictionary.html", readLines(file.path(output, "index.html")), fixed = TRUE)))
  stopifnot(any(grepl("Data dictionary", readLines(file.path(output, "dictionary.html")), fixed = TRUE)))
} else {
  stopifnot(file.exists(file.path(output, "index.html")))
}
cat(normalizePath(file.path(output, "manifest.json")), "\n")
