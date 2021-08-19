# Variation on pin_registry use for versioning

pin_manifest_get <- function(path) {
  manifest <- list()

  data_txt <- file.path(path, "data.txt")
  if (file.exists(data_txt)) {
    manifest <- suppressWarnings(yaml::read_yaml(data_txt, eval.expr = FALSE))
  }

  if (is.null(manifest$type)) manifest$type <- "files"

  manifest
}

pin_manifest_update <- function(path, manifest) {
  data_txt <- file.path(path, "data.txt")

  manifest <- write_yaml(manifest, data_txt)

  manifest
}

pin_manifest_exists <- function(path) {
  identical(file.exists(file.path(path, "data.txt")), TRUE)
}

pin_manifest_create <- function(path, metadata, files) {
  entries <- c(list(
    path = files
  ), metadata)

  entries[sapply(entries, is.null)] <- NULL

  fs::dir_create(path)
  write_yaml(entries, file.path(path, "data.txt"))
}

# retrieve a list of files to download
pin_manifest_download <- function(path, namemap = FALSE) {
  manifest <- pin_manifest_get(path)

  if (is.null(manifest$path)) {
    return(NULL)
  }

  if (manifest$type == "table") {
    rds_match <- grepl(".*.rds", manifest$path)
    downloads <- manifest$path[rds_match]
  } else {
    downloads <- manifest$path
  }

  if (identical(namemap, TRUE)) {
    mapped <- as.list(downloads)

    if (!is.null(manifest$filenames)) {
      names(mapped) <- sapply(mapped, function(e) {
        if (e %in% names(manifest$filenames)) manifest$filenames[e] else ""
      })
    }

    mapped
  }
  else {
    downloads
  }
}

pin_manifest_merge <- function(base_manifest, resource_manifest) {
  # path requires special merge
  if (!is.null(resource_manifest$path) &&
    !is.null(base_manifest$path) &&
    !grepl("https?://", base_manifest$path)) {
    base_manifest$path <- file.path(base_manifest$path, resource_manifest$path)
  }

  base_manifest <- c(base_manifest, resource_manifest)

  # remove duplicates
  base_manifest[duplicated(names(base_manifest))] <- NULL

  base_manifest
}
