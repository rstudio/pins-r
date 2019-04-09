local_random_string <- function(prefix, suffix = "") {
  paste0(basename(tempfile(prefix)), suffix)
}

local_entries_path <- function() {
  file.path(pins_local_path("local"), "config.yml")
}

local_load_entries <- function() {
  entries_path <- local_entries_path()

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

local_save_entries <- function(entries) {
  yaml::write_yaml(entries, local_entries_path())
}

pin_create.local <- function(board, x, name, description, type, metadata) {
  path <- pins_local_path("local")
  entries <- local_load_entries()

  path <- file.path(path, local_random_string("dataset_", ".rds"))

  saveRDS(x, path)

  if (identical(entries, NULL)) entries <- list()

  entries[[length(entries) + 1]] <- list(
    name = name,
    description = description,
    path = path,
    type = type,
    metadata = as.character(metadata)
  )

  local_save_entries(entries)
}

pin_find.local <- function(board, text) {
  entries <- local_load_entries()

  names <- sapply(entries, function(e) e$name)
  descriptions <- sapply(entries, function(e) e$description)
  types <- sapply(entries, function(e) if (is.null(e$type)) "table" else e$type)
  metadata <- sapply(entries, function(e) if (is.null(e$metadata)) "" else e$metadata)

  data.frame(
    name = names,
    description = descriptions,
    type = types,
    metadata = metadata,
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
