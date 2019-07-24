board_initialize.folder <- function(board, ...) {
  if (!dir.exists("pins")) dir.create("pins")
  board
}

board_pin_create.folder <- function(board, path, name, description, type, metadata, ...) {
  folder <- file.path("pins", paste(name, type, sep = "."))

  dir.create(folder)
  file.copy(dir(path, recursive = TRUE, full.names = TRUE), folder)
}

board_pin_get.folder <- function(board, name) {
  path <- file.path("pins", dir("pins", name))
  attr(path, "pin_type") <- gsub("pins[^\\.]+\\.", "", path)
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
