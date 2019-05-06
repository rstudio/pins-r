board_initialize.packages <- function(board, ...) {
  board
}

pin_find.packages <- function(board, text, ...) {
  if (is.null(text)) {
    return(
      data.frame(name = c(), description = c(), type = c(), metadata = c())
    )
  }

  crandatasets <- get_crandatasets()

  parts <- strsplit(text, "_")[[1]]
  if (length(parts) > 1) {
    # remove package name
    text <- paste(parts[2:length(parts)], collapse = "_")
  }

  find_names <- grepl(text, crandatasets$dataset)
  find_description <- grepl(text, crandatasets$description)
  package_pins <- crandatasets[find_names | find_description,]

  if (length(package_pins$dataset) > 0) {
    data.frame(
      name = paste(package_pins$package, package_pins$dataset, sep = "_"),
      description = paste(gsub(" ?\\.$", "", package_pins$description), "from", package_pins$package, "package."),
      type = rep("table", length(package_pins$dataset)),
      metadata = package_pins$metadata
    )
  }
  else {
    data.frame(name = c(), description = c(), type = c(), metadata = c())
  }
}

pin_retrieve.packages <- function(board, name, details) {
  parts <- strsplit(name, "_")[[1]]

  if (length(parts) == 1) stop("Invalid '", name, "' pin name.")

  crandatasets <- get_crandatasets()

  package <- parts[1]
  name <- paste(parts[2:length(parts)], collapse = "_")

  package_pin <- crandatasets[which(crandatasets$package == package, crandatasets$dataset == name),]
  packages_path <- pins_local_path("packages")

  package_path <- dir(
    packages_path,
    pattern = package_pin$package,
    full.names = TRUE)[1]

  if (!dir.exists(package_path)) {

    if (!dir.exists(packages_path)) dir.create(packages_path, recursive = TRUE)

    download.packages(package_pin$package, packages_path, repos = "https://cran.rstudio.com/")

    tar <- dir(
      packages_path,
      pattern = paste0(package_pin$package, ".*.tar.gz"),
      full.names = TRUE)[1]

    untar(tar, exdir = packages_path)
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

  get(load(data_file))
}

get_crandatasets <- function() {
  if (is.null(.globals$datasets)) {
    .globals$datasets <- new.env()
  }

  if (is.null(.globals$datasets$crandatasets)) {
    data(crandatasets, envir = .globals$datasets)
  }

  .globals$datasets$crandatasets
}
