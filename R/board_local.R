board_initialize.local <- function(board, cache, ...) {
  if (!dir.exists(board$cache)) dir.create(board$cache, recursive = TRUE)

  board
}

guess_extension_from_path <- function(path) {
  if (dir.exists(path)) {
    all_files <- dir(path, recursive = TRUE)
    all_files <- Filter(function(x) !grepl("data\\.txt", x), all_files)

    path <- all_files[[1]]
  }

  tools::file_ext(path)
}

board_pin_create.local <- function(board, path, name, metadata, ...) {
  final_path <- pin_storage_path(component = board$name, name = name)

  unlink(final_path, recursive = TRUE)
  dir.create(final_path)

  file.copy(dir(path, full.names = TRUE) , final_path, recursive = TRUE)

  versions <- NULL
  if (board_versions_enabled(board)) {
    version_path <- pin_versions_path(component = board$name, name = name)
    entries <- pin_registry_retrieve_maybe(name = name, component = board$name)

    if (dir.exists(version_path)) unlink(version_path, recursive = TRUE)
    dir.create(version_path, recursive = TRUE)

    file.copy(dir(path, full.names = TRUE), version_path, recursive = TRUE)

    versions <- c(entries$versions, list(pin_registry_relative(version_path, component = board$name)))
  }

  # reduce index size
  metadata$columns <- NULL

  pin_registry_update(
    name = name,
    params = c(list(
      path = pin_registry_relative(final_path, component = board$name),
      versions = versions
    ), metadata),
    component = board$name)
}

board_pin_find.local <- function(board, text, ...) {
  results <- pin_registry_find(text, board$name)

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    path <- pin_registry_absolute(metadata$path, component = board$name)
    extended <- pin_manifest_get(path)
    merged <- pin_manifest_merge(metadata, extended)

    results$metadata <- as.character(jsonlite::toJSON(merged, auto_unbox = TRUE))
  }

  results
}

board_pin_get.local <- function(board, name, ...) {
  entry <- pin_registry_retrieve(name, board$name)
  pin_registry_absolute(entry$path, component = board$name)
}

board_pin_remove.local <- function(board, name) {
  pin_registry_remove(name, board$name)
}

board_pin_versions.local <- function(board, name) {

}
