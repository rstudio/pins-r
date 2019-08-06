pin_registry_path <- function(component) {
  file.path(board_local_storage(component), "config.yml")
}

pin_registry_load_entries <- function(component) {
  entries_path <- pin_registry_path(component)

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

pin_registry_save_entries <- function(entries, component) {
  yaml::write_yaml(entries, pin_registry_path(component))
}

pin_registry_create <- function(name, description, type, metadata, component) {
  if (is.null(description)) description <- ""

  entries <- pin_registry_load_entries(component)

  path <- file.path(board_local_storage(component), name)
  dir.create(path, recursive = TRUE)

  if (identical(entries, NULL)) entries <- list()

  entries[[length(entries) + 1]] <- list(
    name = name,
    description = description,
    path = path,
    type = type,
    metadata = as.character(metadata)
  )

  pin_registry_save_entries(entries, component)

  path
}

pin_registry_update <- function(name, component, metadata) {
  entries <- pin_registry_load_entries(component)

  entries <- lapply(entries, function(e) {
    if (identical(e$name, name)) {
      e$metadata <- metadata
    }

    e
  })

  pin_registry_save_entries(entries, component)
}

pin_registry_find <- function(text, component) {
  entries <- pin_registry_load_entries(component)

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

pin_registry_retrieve <- function(name, component) {
  entries <- pin_registry_load_entries(component)

  names <- sapply(entries, function(e) e$name)
  paths <- sapply(entries, function(e) e$path)
  type <- sapply(entries, function(e) e$type)
  metadata <- sapply(entries, function(e) e$metadata)

  entries <- data.frame(
    name = names,
    path = paths,
    type = type,
    metadata = metadata,
    stringsAsFactors = FALSE
  )

  entry <- entries[entries$name == name, ]

  if (nrow(entry) != 1) stop("Pin '", name, "' not found in '", component, "' board.")

  attr(entry$path, "pin_type") <- as.character(entry$type)
  if (!is.null(entry$metadata)) attr(entry$path, "pin_metadata") <- jsonlite::fromJSON(as.character(entry$metadata))

  entry$path
}

pin_registry_remove <- function(name, component, unlink = TRUE) {
  entries <- pin_registry_load_entries(component)

  remove <- Filter(function(x) x$name == name, entries)
  if (length(remove) > 0)
    remove <- remove[[1]]
  else
    return()

  entries <- Filter(function(x) x$name != name, entries)

  if (unlink) unlink(remove$path, recursive = TRUE)

  pin_registry_save_entries(entries, component)
}
