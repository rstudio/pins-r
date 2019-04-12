yaml_random_string <- function(prefix, suffix = "") {
  paste0(basename(tempfile(prefix)), suffix)
}

yaml_entries_path <- function(component) {
  file.path(pins_local_path(component), "config.yml")
}

yaml_load_entries <- function(component) {
  entries_path <- yaml_entries_path(component)

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

yaml_save_entries <- function(entries, component) {
  yaml::write_yaml(entries, yaml_entries_path(component))
}

pin_create_yaml <- function(name, description, type, metadata, component) {
  path <- pins_local_path(component)
  entries <- yaml_load_entries(component)

  path <- file.path(path, yaml_random_string("dataset_", ".rds"))

  if (identical(entries, NULL)) entries <- list()

  entries[[length(entries) + 1]] <- list(
    name = name,
    description = description,
    path = path,
    type = type,
    metadata = as.character(metadata)
  )

  yaml_save_entries(entries, component)

  path
}

pin_find_yaml <- function(text, component) {
  entries <- yaml_load_entries(component)

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

pin_retrieve_yaml <- function(name, component) {
  entries <- yaml_load_entries(component)

  names <- sapply(entries, function(e) e$name)
  paths <- sapply(entries, function(e) e$path)

  entries <- data.frame(
    name = names,
    path = paths,
    stringsAsFactors = FALSE
  )

  entry <- entries[entries$name == name, ]

  if (nrow(entry) != 1) stop("Pin '", name, "' not found in '", component, "' board.")

  entry$path
}

pin_remove_yaml <- function(name, component) {
  entries <- yaml_load_entries(component)

  remove <- Filter(function(x) x$name == name, entries)
  if (length(remove) > 0)
    remove <- remove[[1]]
  else
    return()

  entries <- Filter(function(x) x$name != name, entries)

  unlink(remove$path)

  yaml_save_entries(entries, component)
}
