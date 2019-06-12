board_yaml_random_string <- function(prefix, suffix = "") {
  paste0(basename(tempfile(prefix)), suffix)
}

board_yaml_entries_path <- function(component) {
  file.path(board_local_storage(component), "config.yml")
}

board_yaml_load_entries <- function(component) {
  entries_path <- board_yaml_entries_path(component)

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

board_yaml_save_entries <- function(entries, component) {
  yaml::write_yaml(entries, board_yaml_entries_path(component))
}

board_yaml_pin_create <- function(name, description, type, metadata, component, extension) {
  if (is.null(description)) description <- ""

  entries <- board_yaml_load_entries(component)

  path <- file.path(board_local_storage(component), board_yaml_random_string("pin_", extension))

  if (identical(entries, NULL)) entries <- list()

  entries[[length(entries) + 1]] <- list(
    name = name,
    description = description,
    path = path,
    type = type,
    metadata = as.character(metadata)
  )

  board_yaml_save_entries(entries, component)

  path
}

board_yaml_pin_find <- function(text, component) {
  entries <- board_yaml_load_entries(component)

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

board_yaml_pin_retrieve <- function(name, component) {
  entries <- board_yaml_load_entries(component)

  names <- sapply(entries, function(e) e$name)
  paths <- sapply(entries, function(e) e$path)
  type <- sapply(entries, function(e) e$type)

  entries <- data.frame(
    name = names,
    path = paths,
    type = type,
    stringsAsFactors = FALSE
  )

  entry <- entries[entries$name == name, ]

  if (nrow(entry) != 1) stop("Pin '", name, "' not found in '", component, "' board.")

  attr(entry$path, "pin_type") <- as.character(entry$type)

  entry$path
}

board_yaml_pin_remove <- function(name, component, unlink = TRUE) {
  entries <- board_yaml_load_entries(component)

  remove <- Filter(function(x) x$name == name, entries)
  if (length(remove) > 0)
    remove <- remove[[1]]
  else
    return()

  entries <- Filter(function(x) x$name != name, entries)

  if (unlink) unlink(remove$path)

  board_yaml_save_entries(entries, component)
}
