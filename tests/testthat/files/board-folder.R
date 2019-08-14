board_initialize.folder <- function(board, ...) {
  if (!dir.exists("pins")) dir.create("pins")
  board
}

board_pin_create.folder <- function(board, path, name, metadata, ...) {
  dir.create(file.path("pins", name), recursive = TRUE, showWarnings = FALSE)
  file.copy(dir(path, full.names = TRUE), file.path("pins", name), recursive = TRUE)
}

board_pin_get.folder <- function(board, name, ...) {
  file.path("pins", name)
}

board_pin_find.folder <- function(board, text, ...) {
  data.frame(name = dir("pins", text), stringsAsFactors = F)
}

board_pin_remove.folder <- function(board, name, ...) {
  unlink(file.path("pins", name), recursive = TRUE)
}

board_register("folder")
