pin_manifest_get <- function(path) {
  manifest <- list()

  pin_json <- file.path(path, "pin.json")
  if (file.exists(pin_json)) {
    manifest <- jsonlite::read_json(pin_json)
  }

  if (is.null(manifest$type)) manifest$type <- "files"

  manifest
}

pin_manifest_create <- function(path, type, description, metadata, files) {
  entries <- list(
    type = type,
    description = description,
    metadata = metadata,
    files = files
  )

  entries[sapply(entries, is.null)] <- NULL

  manifest <- jsonlite::toJSON(
    entries,
    auto_unbox = TRUE)

  writeLines(manifest, file.path(path, "pin.json"))
}
