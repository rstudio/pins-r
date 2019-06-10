board_initialize.local <- function(board, ...) {
  board
}

board_create_pin.local <- function(board, x, name, description, type, metadata, file) {
  pin_remove_yaml(name, component = "local", TRUE)

  extension <- "rds"
  if (is.character(x)) extension <- tools::file_ext(x)

  local_path <- pin_create_yaml(
    name = name,
    description = description,
    type = type,
    metadata = metadata,
    component = "local",
    extension = paste0(".", extension))

  if (is.character(x)) {
    file.copy(x, local_path)
  }
  else {
    saveRDS(x, local_path, version = 2)
  }

  pin_get(name, board$name)
}

board_find_pin.local <- function(board, text, ...) {
  pin_find_yaml(text, "local")
}

board_pin_get.local <- function(board, name, details) {
  path <- pin_retrieve_yaml(name, "local")

  if (identical(pin_type(path), "files")) path else readRDS(path)
}

board_remove_pin.local <- function(board, name) {
  pin_remove_yaml(name, "local")
}
