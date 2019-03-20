# Provides support to find and index CRAN datasets, supports running
# locally or in Spark clusters.
#
# Local:
#   cran_index <- cran_find_local()
#
# Cluster:
#   library(sparklyr)
#   sc <- spark_connect(master = "yarn", config = cran_find_config())
#
#   data <- cran_find_datasets(sc, 100)
#   data <- cran_find_datasets(sc, 10^5, 10^3)
#
#   cran_index <- data %>% collect()
#
# Saving:
#   cran_save_dataset(cran_index)

cran_process_file <- function(package_path, file_path) {
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
          "\n|^ *\\\"?[ \n]*|[ \n]*\\\"? +$",
          "",
          paste(as.character(dataset_title[[1]]), collapse = " "))
        dataset_title <- gsub("  +", " ", dataset_title)
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

cran_process_package <- function(package) {
  results <- data.frame(name = c(), description = c())

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
      cran_process_file(package_path, dataset_path)
    }, error = function(e) {
      data.frame(name = paste("error", package, sep = ":"), description = e$message)
    })

    results <- rbind(
      results,
      new_result
    )
  }

  results
}

cran_process_packages <- function(packages) {
  if (!dir.exists("packages")) dir.create("packages")

  results <- data.frame(name = c(), description = c())

  for (package in packages) {
    new_result <- tryCatch({
      cran_process_package(package)
    }, error = function(e) {
      data.frame(name = paste("error", package, sep = ":"), description = e$message)
    })

    results <- rbind(
      results,
      new_result
    )
  }

  results
}

cran_find_datasets <- function(sc,
                               samples = 2,
                               repartition = sc$config[["sparklyr.shell.num-executors"]]) {
  pkgnames <- available.packages()[,1]

  packages <- copy_to(
    sc,
    data.frame(package = pkgnames[1:samples]),
    repartition = ifelse(is.null(repartition), 0, repartition),
    overwrite = T)

  # package dependencies
  context <- list(
    cran_process_packages = cran_process_packages,
    cran_process_package = cran_process_package,
    cran_process_file = cran_process_file
  )

  packages %>% spark_apply(
    function(df, context) {
      for (name in names(context)) assign(name, context[[name]], envir = .GlobalEnv)
      cran_process_packages(df$package)
    },
    context = context,
    columns = list(name = "character", description = "character"),
    name = "cran_datasets")
}

cran_find_local <- function(samples = 2) {
  cran_process_packages(pkgnames[1:samples])
}

cran_find_config <- function(workers = 3, worker_cpus = 8) {
  config <- spark_config()

  config["sparklyr.shell.driver-memory"] <- "8g"
  config["sparklyr.shell.executor-memory"] <- "1g"
  config["sparklyr.shell.executor-cores"] <- 1
  config["sparklyr.shell.num-executors"] <- workers * worker_cpus

  config
}

cran_save_dataset <- function(cran_index) {
  if (!dir.exists("data")) dir.create("data")

  crandatasets <- dplyr::transmute(
    cran_index,
    package = gsub(":.*", "", name),
    dataset = gsub(".*:", "", name),
    description = description
  )

  save(crandatasets, file = "data/crandatasets.rda")
}
