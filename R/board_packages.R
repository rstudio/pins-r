board_initialize.packages <- function(board, ...) {
  # use local board cache to avoid 'board_register("packages", cache = tempfile())' examples
  if (identical(board$cache, board_cache_path())) board$cache <- dirname(board_local_storage("local"))

  board
}

board_pin_find.packages <- function(board, text, ...) {
  if (is.null(text)) text <- ""
  cranfiles <- get_cranfiles()

  parts <- strsplit(text, "/")[[1]]
  if (length(parts) > 1) {
    # remove package name
    text <- paste(parts[2:length(parts)], collapse = "/")
  }

  find_names <- grepl(text, cranfiles$dataset, ignore.case = TRUE)
  find_description <- grepl(text, cranfiles$description, ignore.case = TRUE)
  package_pins <- cranfiles[find_names | find_description,]

  if (length(package_pins$dataset) > 0) {
    data.frame(
      name = paste(package_pins$package, package_pins$dataset, sep = "/"),
      description = paste(gsub(" ?\\.$", "", package_pins$description), "from", package_pins$package, "package."),
      type = rep("table", length(package_pins$dataset)),
      metadata = package_pins$metadata,
      stringsAsFactors = FALSE
    )
  }
  else {
    board_empty_results()
  }
}

packages_repo_default <- function() {
  repos <- getOption("repos")["CRAN"]
  if (is.null(repos) || length(repos) == 0 || is.na(repos) || identical(as.character(repos), "@CRAN@")) repos <- "https://cran.rstudio.com/"

  repos
}

packages_download <- function(resource_path, package_pin, name) {
  dir.create(resource_path, recursive = TRUE)

  temp_path <- tempfile()
  dir.create(temp_path)
  on.exit(unlink(temp_path, recursive = TRUE))

  repos <- packages_repo_default()

  progress <- function(e) e
  if (pins_show_progress())
    progress <- function(e) utils::capture.output(e, type = "message")

  progress(result <- utils::download.packages(package_pin$package, temp_path, repos = repos))

  tar <- dir(
    temp_path,
    pattern = paste0(package_pin$package, ".*.tar.gz"),
    full.names = TRUE)[1]

  utils::untar(tar, exdir = temp_path)
  unlink(tar)

  temp_package <- dir(
    temp_path,
    pattern = package_pin$package,
    full.names = TRUE)[1]

  temp_file <- dir(
    file.path(temp_package, "data"),
    pattern = name,
    full.names = TRUE)[1]

  file.copy(temp_file, resource_path)

  metadata <- jsonlite::fromJSON(package_pin$metadata)
  metadata$type <- "package"
  metadata$description <- package_pin$description

  pin_manifest_create(resource_path, metadata, "")
}

board_pin_get.packages <- function(board, name, ...) {
  parts <- strsplit(name, "/")[[1]]

  if (length(parts) == 1) stop("Invalid '", name, "' pin name.")

  cranfiles <- get_cranfiles()

  package <- parts[1]
  name <- paste(parts[2:length(parts)], collapse = "/")

  package_pin <- cranfiles[which(cranfiles$package == package & cranfiles$dataset == name),]
  if (nrow(package_pin) == 0) stop("Pin '", name, "' does not exist in packages board.")

  packages_path <- board_local_storage("packages")

  resource_path <- file.path(packages_path, package, name)

  if (!dir.exists(resource_path) || length(dir(resource_path)) == 0) {
    packages_download(resource_path, package_pin, name)
  }

  resource_path
}

get_cranfiles <- function() {
  if (is.null(.globals$datasets)) {
    .globals$datasets <- new.env()
  }

  if (is.null(.globals$datasets$cranfiles)) {
    utils::data("cranfiles", envir = .globals$datasets, package = "pins")
  }

  if (is.null(.globals$datasets$cranfiles)) {
    stop("Failed to load 'cranfiles' dataset")
  }

  .globals$datasets$cranfiles
}
