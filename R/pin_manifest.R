pin_manifest_get <- function(path) {
  manifest <- list()

  data_txt <- file.path(path, "data.txt")
  if (file.exists(data_txt)) {
    manifest <- yaml::read_yaml(data_txt, eval.expr = FALSE)
  }

  if (is.null(manifest$type)) manifest$type <- "files"

  manifest
}

pin_manifest_create <- function(path, metadata, files) {
  entries <- c(list(
    path = files
  ), metadata)

  entries[sapply(entries, is.null)] <- NULL

  yaml::write_yaml(entries, file.path(path, "data.txt"))
}

# retrieve a list of files dow download
pin_manifest_download <- function(path) {
  manifest <- pin_manifest_get(path)

  if (is.null(manifest$path)) return(NULL)
  pin_fetch(structure(manifest$path, class = manifest$type))
}
