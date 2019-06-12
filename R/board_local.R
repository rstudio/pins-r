board_initialize.local <- function(board, ...) {
  board
}

board_pin_create.local <- function(board, path, name, description, type, metadata, file) {
  on.exit(board_connect(board$name))

  board_yaml_pin_remove(name, component = "local", TRUE)

  extension <- tools::file_ext(path)

  local_path <- board_yaml_pin_create(
    name = name,
    description = description,
    type = type,
    metadata = metadata,
    component = "local",
    extension = paste0(".", extension))

  file.copy(path, local_path)

  pin_get(name, board$name)
}

board_pin_find.local <- function(board, text, ...) {
  board_yaml_pin_find(text, "local")
}

board_pin_get.local <- function(board, name, details) {
  board_yaml_pin_retrieve(name, "local")
}

board_pin_remove.local <- function(board, name) {
  board_yaml_pin_remove(name, "local")
}
