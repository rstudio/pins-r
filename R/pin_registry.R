pin_registry_read <- function(board) {
  stopifnot(is.board(board))

  path <- pin_registry_path(board)
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

  yaml::write_yaml(unname(entries), pin_registry_path(board))
}

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

pin_registry_remove <- function(board, name, unlink = TRUE) {
  stopifnot(is.board(board))
  local_registry_lock(board)

  entries <- pin_registry_read(board)
  name <- pin_registry_qualify_name(name, entries)

  if (!name %in% names(entries)) {
    return()
  }

  # Delete corresponding file
  path <- fs::path_abs(entries[[name]]$path, fs::path(board$cache, board$name))
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



# Lock registry file in case being written by two packages simultaneously
local_registry_lock <- function(board, .env = parent.frame()) {
  path <- paste0(pin_registry_path(board), ".lock")

  lock <- filelock::lock(path, timeout = getOption("pins.lock.timeout", Inf))
  withr::defer(filelock::unlock(lock), .env)
}

pin_registry_path <- function(board) {
  path <- file.path(board$cache, board$name)
  fs::dir_create(path)

  fs::path(path, "data.txt")
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

# other stuff that doesn't really belong here -----------------------------

pin_storage_path <- function(board, name) {
  path <- fs::path(pin_registry_path(board), name)
  fs::dir_create(path)

  path
}


pin_registry_relative <- function(path, base_path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  base_path <- normalizePath(base_path, winslash = "/", mustWork = FALSE)

  if (startsWith(path, base_path)) {
    path <- substr(path, nchar(base_path) + 1, nchar(path))
  }

  relative <- gsub("^/", "", path)

  relative
}

pin_registry_absolute <- function(path, component) {
  base_path <- tools::file_path_as_absolute(board_local_storage(component))

  if (startsWith(path, base_path)) {
    path
  } else {
    normalizePath(file.path(base_path, path), mustWork = FALSE)
  }
}

# testing -----------------------------------------------------------------

local_registry_board <- function(name = "test", env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  new_board("test", name, cache = path)
}
