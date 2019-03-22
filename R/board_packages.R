board_initialize.packages <- function(...) {

}

pin_find.packages <- function(board, text) {
  if (is.null(text)) {
    return(
      data.frame(name = c(), description = c())
    )
  }

  find_packages <- grepl(text, crandatasets$package)
  find_names <- grepl(text, crandatasets$dataset)
  find_description <- grepl(text, crandatasets$description)
  package_pins <- crandatasets[find_packages | find_names | find_description,]

  data.frame(
    name = package_pins$dataset,
    description = paste(gsub(" ?\\.$", "", package_pins$description), "from", package_pins$package, "package.")
  )
}

pin_retrieve.packages <- function(name) {
  package_pin <- crandatasets[crandatasets$dataset == name,][1,]
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
