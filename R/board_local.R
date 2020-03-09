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
  board_versions_create(board, name = name, path = path)

  final_path <- pin_storage_path(component = board$name, name = name)

  delete <- dir(final_path, full.names = TRUE)
  delete <- delete[!grepl("/_versions$", delete)]
  unlink(delete, recursive = TRUE)
  if (!dir.exists(final_path)) dir.create(final_path)

  file.copy(dir(path, full.names = TRUE) , final_path, recursive = TRUE)

  # reduce index size
  metadata$columns <- NULL

  base_path <- board_local_storage(board$name)

  pin_registry_update(
    name = name,
    params = c(list(
      path = pin_registry_relative(final_path, base_path = base_path)
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

board_pin_get.local <- function(board, name, version = NULL, ...) {
  path <- pin_registry_retrieve_path(name, board$name)

  if (!is.null(version)) {
    manifest <- pin_manifest_get(pin_registry_absolute(path, board$name))

    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    path <- file.path(name, version)
  }

  pin_registry_absolute(path, component = board$name)
}

board_pin_remove.local <- function(board, name) {
  pin_registry_remove(name, board$name)
}

board_pin_versions.local <- function(board, name) {
  board_versions_get(board, name)
}
