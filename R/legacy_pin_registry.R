# Read/write whole registry -----------------------------------------------

pin_registry_read <- function(board) {
  stopifnot(is.board(board))

  path <- pin_registry_path(board, "data.txt")
  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path, eval.expr = FALSE)
    if (length(yaml) > 0) {
      names(yaml) <- vapply(yaml, function(e) e$name, character(1))
    }
    yaml
  } else {
    list()
  }
}

pin_registry_write <- function(board, entries) {
  stopifnot(is.board(board))

  write_yaml(unname(entries), pin_registry_path(board, "data.txt"))
}

# Lock registry file to prevent multi-process race conditions
local_registry_lock <- function(board, env = parent.frame()) {
  check_installed("filelock")
  path <- paste0(pin_registry_path(board, "data.txt"), ".lock")

  lock <- filelock::lock(path, timeout = getOption("pins.lock.timeout", Inf))
  withr::defer(filelock::unlock(lock), env)
}

# Get/set individual components -------------------------------------------

# TODO: declare standard parameters that every pin needs to use
pin_registry_update <- function(board, name, metadata = list()) {
  stopifnot(is.board(board))
  local_registry_lock(board)

  entries <- pin_registry_read(board)
  name <- pin_registry_qualify_name(name, entries)

  if (name %in% names(entries)) {
    entries[[name]] <- utils::modifyList(entries[[name]], metadata)
  } else {
    metadata$name <- name
    entries[[length(entries) + 1]] <- metadata
  }

  pin_registry_write(board, entries)
}

pin_registry_retrieve <- function(board, name) {
  stopifnot(is.board(board))

  entries <- pin_registry_read(board)
  name <- pin_registry_qualify_name(name, entries)

  if (!name %in% names(entries)) {
    stop("Pin '", name, "' not found in board '", board$name, "'.")
  }
  entries[[name]]
}

pin_registry_find <- function(board, text) {
  stopifnot(is.board(board))

  entries <- pin_registry_read(board)
  results <- pin_results_from_rows(entries)

  if (!is.null(text)) {
    results <- results[grepl(text, results$name, fixed = TRUE), , drop = FALSE]
  }
  results
}

pin_results_from_rows <- function(entries) {
  get_metadata <- function(e) {
    extra <- e[setdiff(names(e), c("name", "description", "type"))]
    jsonlite::toJSON(extra, auto_unbox = TRUE)
  }

  data.frame(
    name = map_chr(entries, ~ .x$name %||% basename(.x$path)),
    description = map_chr(entries, ~ .x$description %||% ""),
    type = map_chr(entries, ~ .x$type %||% "files"),
    metadata = map_chr(entries, get_metadata),
    stringsAsFactors = FALSE
  )
}

pin_registry_remove <- function(board, name, unlink = TRUE) {
  stopifnot(is.board(board))
  local_registry_lock(board)

  entries <- pin_registry_read(board)
  name <- pin_registry_qualify_name(name, entries)

  if (!name %in% names(entries)) {
    return()
  }

  # Delete corresponding file
  path <- fs::path_abs(entries[[name]]$path, pin_registry_path(board))
  if (unlink) {
    unlink(path, recursive = TRUE)
  }

  entries[[name]] <- NULL
  pin_registry_write(board, entries)
}

pin_register_reset_cache <- function(board, name) {
  stopifnot(is.board(board))
  local_registry_lock(board)

  entries <- pin_registry_read(board)
  name <- pin_registry_qualify_name(name, entries)

  if (!name %in% names(entries)) {
    return()
  }

  entries[[name]]$cache <- list()
  pin_registry_write(board, entries)
}


# helpers -----------------------------------------------------------------

pin_registry_path <- function(board, ...) {
  if (is.na(board$cache)) {
    path <- board$path
  } else {
    path <- board$cache
  }
  fs::path(path, ...)
}

# I think this is used so that the rsconnect board can match x to any user
pin_registry_qualify_name <- function(name, entries) {
  name_pattern <- if (grepl("/", name)) paste0("^", name, "$") else paste0(".*/", name, "$")
  name_candidate <- names(entries)[grepl(name_pattern, names(entries))]

  if (length(name_candidate) == 1) {
    name <- name_candidate
  }

  name
}
