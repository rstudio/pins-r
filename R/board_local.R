board_initialize.local <- function(board, ...) {
  board
}

pin_create.local <- function(board, x, name, description, type, metadata) {
  path <- pin_create_yaml(
    name = name,
    description = description,
    type = type,
    metadata = metadata,
    component = "local",
    extension = ".rds")

  saveRDS(x, path)
}

pin_find.local <- function(board, text) {
  pin_find_yaml(text, "local")
}

pin_retrieve.local <- function(board, name) {
  path <- pin_retrieve_yaml(name, "local")
  readRDS(path)
}

pin_remove.local <- function(board, name) {
  pin_remove_yaml(name, "local")
}
