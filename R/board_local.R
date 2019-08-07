board_initialize.local <- function(board, ...) {
  board
}

guess_extension_from_path <- function(path) {
  if (dir.exists(path)) {
    all_files <- dir(path, recursive = TRUE)
    all_files <- Filter(function(x) !grepl("pin\\.json", x), all_files)

    path <- all_files[[1]]
  }

  tools::file_ext(path)
}

board_pin_create.local <- function(board, path, name, description, type, metadata, file, ...) {
  on.exit(board_connect(board$name))

  if (grepl("^http", path)) {
    final_path <- pin_download(path, name, "local", ...)
  }
  else {
    final_path <- pin_registry_update(name = name, component = "local")

    unlink(final_path, recursive = TRUE)
    dir.create(final_path)

    if (dir.exists(path)) {
      file.copy(dir(path, recursive = TRUE, full.names = TRUE) , final_path)
    }
    else {
      file.copy(path, final_path)
    }
  }

  pin_registry_update(
    name = name,
    params = list(
      path = final_path,
      description = description,
      type = type,
      metadata = jsonlite::toJSON(metadata, auto_unbox = TRUE)
    ),
    component = "local")
}

board_pin_find.local <- function(board, text, ...) {
  pin_registry_find(text, "local")
}

board_pin_get.local <- function(board, name) {
  entry <- pin_registry_retrieve(name, "local")

  attr(entry$path, "pin_type") <- as.character(entry$type)
  if (!is.null(entry$metadata)) attr(entry$path, "pin_metadata") <- jsonlite::fromJSON(as.character(entry$metadata))

  entry$path
}

board_pin_remove.local <- function(board, name) {
  pin_registry_remove(name, "local")
}
