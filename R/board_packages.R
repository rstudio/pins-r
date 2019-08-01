board_initialize.packages <- function(board, ...) {
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

  find_names <- grepl(text, cranfiles$dataset)
  find_description <- grepl(text, cranfiles$description)
  package_pins <- cranfiles[find_names | find_description,]

  if (length(package_pins$dataset) > 0) {
    data.frame(
      name = paste(package_pins$package, package_pins$dataset, sep = "/"),
      description = paste(gsub(" ?\\.$", "", package_pins$description), "from", package_pins$package, "package."),
      type = rep("table", length(package_pins$dataset)),
      metadata = package_pins$metadata
    )
  }
  else {
    data.frame(name = c(), description = c(), type = c(), metadata = c())
  }
}

board_pin_get.packages <- function(board, name) {
  parts <- strsplit(name, "/")[[1]]

  if (length(parts) == 1) stop("Invalid '", name, "' pin name.")

  cranfiles <- get_cranfiles()

  package <- parts[1]
  name <- paste(parts[2:length(parts)], collapse = "/")

  package_pin <- cranfiles[which(cranfiles$package == package, cranfiles$dataset == name),]
  if (nrow(package_pin) == 0) stop("Pin '", name, "' does not exist in packages board.")

  packages_path <- board_local_storage("packages")

  package_path <- dir(
    packages_path,
    pattern = package_pin$package,
    full.names = TRUE)[1]

  if (!dir.exists(package_path)) {

    if (!dir.exists(packages_path)) dir.create(packages_path, recursive = TRUE)

    utils::download.packages(package_pin$package, packages_path, repos = "https://cran.rstudio.com/")

    tar <- dir(
      packages_path,
      pattern = paste0(package_pin$package, ".*.tar.gz"),
      full.names = TRUE)[1]

    utils::untar(tar, exdir = packages_path)
    unlink(tar)

    package_path <- dir(
      packages_path,
      pattern = package_pin$package,
      full.names = TRUE)[1]
  }

  data_file <- dir(
    file.path(package_path, "data"),
    pattern = name,
    full.names = TRUE)[1]

  attr(data_file, "pin_type") <- "package"
  data_file
}

get_cranfiles <- function() {
  if (is.null(.globals$datasets)) {
    .globals$datasets <- new.env()
  }

  if (is.null(.globals$datasets$cranfiles)) {
    utils::data("cranfiles", envir = .globals$datasets)
  }

  .globals$datasets$cranfiles
}
