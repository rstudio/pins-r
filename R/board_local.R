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
  final_path <- pin_registry_update(name = name, component = board$name)

  unlink(final_path, recursive = TRUE)
  dir.create(final_path)

  file.copy(dir(path, full.names = TRUE) , final_path, recursive = TRUE)

  # reduce index size
  metadata$columns <- NULL

  pin_registry_update(
    name = name,
    params = c(list(
      path = final_path
    ), metadata),
    component = board$name)
}

board_pin_find.local <- function(board, text, ...) {
  results <- pin_registry_find(text, board$name)

  if (nrow(results) == 1) {
    metadata <- jsonlite::fromJSON(results$metadata)
    extended <- pin_manifest_get(metadata$path)

    results$metadata <- as.character(jsonlite::toJSON(c(metadata, extended), auto_unbox = TRUE))
  }

  results
}

board_pin_get.local <- function(board, name, ...) {
  entry <- pin_registry_retrieve(name, board$name)
  entry$path
}

board_pin_remove.local <- function(board, name) {
  pin_registry_remove(name, board$name)
}
