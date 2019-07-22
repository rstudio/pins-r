pin_manifest_get <- function(path) {
  manifest <- list(
    type = "files"
  )

  pin_json <- file.path(path, "pin.json")
  if (file.exists(pin_json)) {
    pin_data <- jsonlite::read_json(pin_json)
    manifest$type <- pin_data$type
  }

  manifest
}

pin_manifest_create <- function(path, type, metadata, files) {
  manifest <- jsonlite::toJSON(
    list(
      type = type,
      metadata = metadata,
      files = files
    ),
    auto_unbox = TRUE)

  writeLines(manifest, file.path(path, "pin.json"))
}
