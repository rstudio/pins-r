
process_file <- function(package_path, file_path) {
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

process_packages <- function(packages, context = NULL) {
  if (!dir.exists("packages")) dir.create("packages")

  results <- data.frame(name = c(), description = c())

  for (package in packages) {
    if (!dir.exists(file.path("packages", package))) {
      download.packages(package, "packages", repos = "https://cran.rstudio.com/")

      tar <- dir("packages", pattern = "*.tar.gz", full.names = TRUE)[1]
      untar(tar, exdir = "packages")

      unlink("packages/*.gz")
    }

    package_path <- file.path("packages", package)
    dataset_paths <- dir(file.path(package_path, "data"), full.names = TRUE)
    for (dataset_path in dataset_paths) {
      new_result <- tryCatch({
        process_file(package_path, dataset_path)
      }, error = function(e) {
        data.frame(name = paste("error", package, sep = ":"), description = e$message)
      })

      results <- rbind(
        results,
        new_result
      )
    }
  }

  results
}

find_datasets_spark <- function(sc, samples = 2) {
  pkgnames <- available.packages()[,1]
  packages <- copy_to(sc, data.frame(package = pkgnames[1:samples]), overwrite = T)

  # package dependencies
  context <- list(
    process_packages = process_packages,
    process_file = process_file
  )

  packages %>% spark_apply(
    function(df, context) {
      for (name in names(context)) assign(name, context[[name]], envir = .GlobalEnv)
      process_packages(df$package, package_tools)
    },
    context = context,
    columns = list(name = "character", description = "character"),
    name = "cran_datasets")
}

find_datasets_local <- function(samples = 2) {
  process_packages(pkgnames[1:samples])
}
