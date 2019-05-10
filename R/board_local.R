board_initialize.local <- function(board, ...) {
  board
}

pin_create.local <- function(board, x, name, description, type, metadata) {
  pin_remove_yaml(name, component = "local", !is_file_pin(x))

  path <- if (is_file_pin(x)) x else NULL

  path <- pin_create_yaml(
    name = name,
    description = description,
    type = type,
    metadata = metadata,
    component = "local",
    extension = ".rds",
    path = path)

  if (!is_file_pin(x)) saveRDS(x, path)
}

pin_find.local <- function(board, text, ...) {
  pin_find_yaml(text, "local")
}

pin_retrieve.local <- function(board, name, details) {
  path <- pin_retrieve_yaml(name, "local")

  if (is_file_pin(path)) path else readRDS(path)
}

pin_remove.local <- function(board, name) {
  pin_remove_yaml(name, "local")
}
