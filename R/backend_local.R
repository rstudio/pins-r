local_path <- function() {
  paths <- list(
    unix = "~/pins",
    windows = "%LOCALAPPDATA%/pins"
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pins.path"), NULL)) path <- getOption("pins.path")

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  path
}

local_random_string <- function(prefix, suffix = "") {
  paste0(basename(tempfile(prefix)), suffix)
}

local_entries_path <- function() {
  file.path(local_path(), "config.yml")
}

local_load_entries <- function() {
  entries_path <- local_entries_path()

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

local_save_entries <- function(entries) {
  yaml::write_yaml(entries, local_entries_path())
}

pin_create.local <- function(board, x, name, description) {
  path <- local_path()
  entries <- local_load_entries()

  path <- file.path(path, local_random_string("dataset_", ".rds"))

  saveRDS(x, path)

  if (identical(entries, NULL)) entries <- list()

  entries[[length(entries) + 1]] <- list(
    name = name,
    description = description,
    path = path
  )

  local_save_entries(entries)
}

pin_find.local <- function(board, text) {
  entries <- local_load_entries()

  names <- sapply(entries, function(e) e$name)
  descriptions <- sapply(entries, function(e) e$description)

  data.frame(
    name = names,
    description = descriptions,
    stringsAsFactors = FALSE
  )
}

pin_retrieve.local <- function(board, name) {
  entries <- local_load_entries()

  names <- sapply(entries, function(e) e$name)
  paths <- sapply(entries, function(e) e$path)

  entries <- data.frame(
    name = names,
    path = paths,
    stringsAsFactors = FALSE
  )

  entry <- entries[entries$name == name, ]

  if (nrow(entry) != 1) stop("Pin '", name, "' not found in '", board$name, "' board.")

  readRDS(entry$path)
}

pin_remove.local <- function(board, name) {
  entries <- local_load_entries()

  remove <- Filter(function(x) x$name == name, entries)
  if (length(remove) > 0)
    remove <- remove[[1]]
  else
    return()

  entries <- Filter(function(x) x$name != name, entries)

  unlink(remove$path)

  local_save_entries(entries)
}

board_initialize.local <- function(...) {

}
