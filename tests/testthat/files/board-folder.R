board_initialize.folder <- function(board, ...) {
  if (!dir.exists("pins")) dir.create("pins")
  board
}

board_pin_create.folder <- function(board, path, name, description, type, metadata, ...) {
  file.copy(path, file.path("pins", paste0(name, ".", type)))

  pin_get(name, board = board$name)
}

board_pin_get.folder <- function(board, name) {
  path <- dir("pins", name, full.names = T)
  attr(path, "pin_type") <- tools::file_ext(path)
  path
}

board_pin_find.folder <- function(board, text) {
  path <- dir("pins", text, full.names = T)
  names <- sapply(strsplit(basename(path), "\\."), function(e) e[1])
  types <- sapply(strsplit(basename(path), "\\."), function(e) e[2])
  empties <- rep("", length(types))

  data.frame(name = names, description = empties, type = types, metadata = empties, stringsAsFactors = F)
}

board_pin_remove.folder <- function(board, name) {
  unlink(file.path("pins", name))
}

board_register("folder")
