
package_tools <- list()

package_tools$process_file <- function(package_path, file_path) {
  dataset_title <- NULL
  file_name <- basename(file_path)
  dataset_name <- tools::file_path_sans_ext(file_name)
  if (tools::file_ext(file_name) %in% c("rda", "RData")) {
    doc_file <- file.path(package_path, "man", paste0(dataset_name, ".Rd"))
    if (file.exists(doc_file)) {
      dataset_doc <- tools::parse_Rd(doc_file)
      dataset_title <- Filter(function(e) identical(attr(e, "Rd_tag"), "\\title"), dataset_doc)
      if (length(dataset_title) > 0) {
        dataset_title <- gsub(
          "\n|^\\\"? +| +\\\"?$",
          "",
          paste(as.character(dataset_title[[1]]), collapse = " "))
        dataset_content <- get(load(file_path))
      }
    }
  }

  if (is.null(dataset_title)) {
    data.frame(name = c(), description = c())
  }
  else {
    data.frame(name = paste(basename(package_path), dataset_name, sep = ":"), description = dataset_title)
  }
}

package_tools$process_packages <- function(packages, package_tools) {
  if (!dir.exists("packages")) dir.create("packages")
  download.packages(packages, "packages", repos = "https://cran.rstudio.com/")

  tars <- dir("packages", full.names = TRUE)
  sapply(tars, function(e) untar(e, exdir = "packages"))

  unlink("packages/*.gz")

  unpacked <- dir("packages", full.names = TRUE, include.dirs = TRUE)

  results <- data.frame(name = c(), description = c())
  for (package in unpacked) {
    dataset_paths <- dir(file.path(package, "data"), full.names = TRUE)
    for (dataset_path in dataset_paths) {
      results <- rbind(
        results,
        package_tools$process_file(package, dataset_path)
      )
    }
  }

  results
}

library(sparklyr)
sc <- spark_connect(master = "yarn")

pkgnames <- available.packages()[,1]
packages <- copy_to(sc, data.frame(package = pkgnames[1:10]), overwrite = T)

packages %>% spark_apply(function(df, package_tools) {
  package_tools$process_packages(df$package, package_tools)
}, context = package_tools, columns = list(name = "character", description = "character"))

