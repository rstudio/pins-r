pin_registry_config <- function(component) {
  file.path(board_local_storage(component), "config.yml")
}

pin_registry_load_entries <- function(component) {
  entries_path <- pin_registry_config(component)

  if (file.exists(entries_path)) yaml::read_yaml(entries_path) else list()
}

pin_registry_save_entries <- function(entries, component) {
  yaml::write_yaml(entries, pin_registry_config(component))
}

pin_registry_path <- function(component, name) {
  path <- file.path(board_local_storage(component), name)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  path
}

pin_registry_update <- function(name, component, params = list()) {
  entries <- pin_registry_load_entries(component)

  path <- pin_registry_path(component, name)

  if (identical(entries, NULL)) entries <- list()

  names <- sapply(entries, function(e) e$name)
  if (name %in% names) {
    index <- which(name == names)
  }
  else {
    index <- length(entries) + 1
    entries[[index]] <- list()
  }

  entries[[index]]$name <- name

  for (param in names(params)) {
    entries[[index]][[param]] <- params[[param]]
  }

  pin_registry_save_entries(entries, component)

  path
}

pin_registry_field <- function(entries, field, default) {
  sapply(entries,
         function(e) if (is.null(e[[field]])) default else e[[field]])
}

pin_registry_find <- function(text, component) {
  entries <- pin_registry_load_entries(component)

  names <- sapply(entries, function(e) e$name)
  descriptions <- pin_registry_field(entries, "description", "")
  types <- pin_registry_field(entries, "type", "files")
  metadata <- pin_registry_field(entries, "metadata", "")

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
  if (!name %in% names) stop("Pin '", name, "' not found in '", component, "' board.")

  entries[[which(names == name)]]
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
