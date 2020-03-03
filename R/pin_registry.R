pin_registry_config <- function(component) {
  file.path(board_local_storage(component), "data.txt")
}

pin_registry_load_entries <- function(component) {
  lock <- pin_registry_lock(component)
  on.exit(pin_registry_unlock(lock))

  entries_path <- pin_registry_config(component)

  if (file.exists(entries_path)) yaml::read_yaml(entries_path, eval.expr = FALSE) else list()
}

pin_registry_save_entries <- function(entries, component) {
  lock <- pin_registry_lock(component)
  on.exit(pin_registry_unlock(lock))

  yaml::write_yaml(entries, pin_registry_config(component))
}

pin_storage_path <- function(component, name) {
  path <- file.path(board_local_storage(component), name)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  path
}

pin_registry_update <- function(name, component, params = list()) {
  lock <- pin_registry_lock(component)
  on.exit(pin_registry_unlock(lock))

  entries <- pin_registry_load_entries(component)
  name <- pin_registry_qualify_name(name, entries)

  path <- pin_storage_path(component, name)

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
    if (identical(params[[param]], list()))
      entries[[index]][[param]] <- NULL
    else
      entries[[index]][[param]] <- params[[param]]
  }

  pin_registry_save_entries(entries, component)

  path
}

pin_registry_find <- function(text, component) {
  lock <- pin_registry_lock(component)
  on.exit(pin_registry_unlock(lock))

  entries <- pin_registry_load_entries(component)

  results <- pin_results_from_rows(entries)

  if (is.character(text)) {
    results <- results[grepl(text, results$name),]
  }

  results
}

pin_registry_retrieve <- function(name, component) {
  lock <- pin_registry_lock(component)
  on.exit(pin_registry_unlock(lock))

  entries <- pin_registry_load_entries(component)
  name <- pin_registry_qualify_name(name, entries)

  names <- sapply(entries, function(e) e$name)
  if (!name %in% names) stop("Pin '", name, "' not found in '", component, "' board.")

  entries[[which(names == name)]]
}

pin_registry_retrieve_maybe <- function(name, component) {
  tryCatch(pin_registry_retrieve(name, component), error = function(e) NULL)
}

pin_registry_remove <- function(name, component, unlink = TRUE) {
  entries <- pin_registry_load_entries(component)
  name <- pin_registry_qualify_name(name, entries)

  remove <- Filter(function(x) x$name == name, entries)
  if (length(remove) > 0)
    remove <- remove[[1]]
  else
    return()

  entries <- Filter(function(x) x$name != name, entries)

  if (unlink) unlink(remove$path, recursive = TRUE)

  pin_registry_save_entries(entries, component)
}

pin_registry_qualify_name <- function(name, entries) {
  names <- sapply(entries, function(e) e$name)
  name_pattern <- if (grepl("/", name)) paste0("^", name, "$") else paste0(".*/", name, "$")
  name_candidate <- names[grepl(name_pattern, names)]

  if (length(name_candidate) == 1) {
    name <- name_candidate
  }

  name
}

pin_registry_lock <- function(component) {
  lock_file <- paste0(pin_registry_config(component), ".lock")
  filelock::lock(lock_file, timeout = getOption("pins.lock.timeout", Inf))
}

pin_registry_unlock <- function(lock) {
  filelock::unlock(lock)
}

