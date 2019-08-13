pin_load.files <- function(path, ...) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)

  files[!grepl("data\\.txt$", files)]
}
