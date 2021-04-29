#' Use a local folder as board
#'
#' @description
#' * `board_folder()` creates a board inside a folder. You can use this to
#'    share files by using a folder on a shared network drive or inside
#'    a DropBox.
#'
#' * `board_local()` creates a board in a system data directory. It's useful
#'    if you want to share pins between R sessions on your computer, and you
#'    don't care where the data lives.
#'
#' * `board_temp()` creates a temporary board that lives in a session
#'    specific temporary directory. It will be automatically deleted once
#'    the current R session ends. It's useful for examples and tests.
#'
#' @inheritParams new_board
#' @param path Path to directory to store pins. Will be created if it
#'   doesn't already exist.
#' @family boards
#' @examples
#' # session-specific local board
#' board <- board_temp()
#' @export
board_folder <- function(path, name = "folder", versions = FALSE) {
  fs::dir_create(path)

  new_board("pins_board_local",
    name = name,
    cache = NA_character_,
    path = path,
    versions = versions
  )
}

#' @export
#' @rdname board_folder
board_local <- function(versions = FALSE) {
  board_folder(rappdirs::user_data_dir("pins"), name = "local", versions = versions)
}

#' @rdname board_folder
#' @export
board_temp <- function(name = "temp", versions = FALSE) {
  board_folder(fs::file_temp("pins-"), name = name, versions = versions)
}

# Methods -----------------------------------------------------------------

#' @export
board_desc.pins_board_local <- function(board, ...) {
  paste0("Path: '", board$path, "'")
}

#' @export
pin_list.pins_board_local <- function(board, ...) {
  fs::path_file(fs::dir_ls(board$path, type = "directory"))
}

#' @export
board_pin_remove.pins_board_local <- function(board, name, ...) {
  check_name(name)
  fs::dir_delete(fs::path(board$path, name))
}

#' @export
board_pin_versions.pins_board_local <- function(board, name, ...) {
  path_pin <- fs::path(board$path, name)
  versions <- read_versions(path_pin) %||% character(0)

  meta <- map(versions, function(v) pin_meta(board, name, version = v))
  date <- rsc_parse_time(map_chr(meta, function(x) x[["date"]] %||% NA_character_))

  tibble::tibble(version = versions, created = date)
}

#' @export
pin_browse.pins_board_local <- function(board, name, version = NULL, ..., cache = FALSE) {
  if (cache) {
    abort("board_local() does not have a cache")
  }
  meta <- pin_meta(board, name, version = version)
  browse_url(meta$local$dir)
}

# pins v1 ----------------------------------------------------------------

#' @export
pin_store.pins_board_local <- function(board, name, path, metadata,
                                              versioned = NULL, ...) {
  check_name(name)
  path_pin <- fs::path(board$path, name)
  versions <- read_versions(path_pin)

  if (length(versions) > 1) {
    versioned <- versioned %||% TRUE
  } else {
    versioned <- versioned %||% board$versions
  }

  if (!versioned) {
    if (length(versions) == 0) {
      pins_inform(paste0("Creating new version '", metadata$pin_hash, "'"))
    } else if (length(versions) == 1) {
      pins_inform(paste0(
        "Replacing version '", versions, "'",
        " with '", metadata$pin_hash, "'"
      ))
      fs::dir_delete(fs::path(path_pin, versions))
      versions <- NULL
    } else {
      abort(c(
        "Pin is versioned, but you have requested a write without versions",
        i = "To un-version a pin, you must delete it"
      ))
    }
  } else {
    pins_inform(paste0("Creating new version '", metadata$pin_hash, "'"))
  }

  path_version <- fs::path(path_pin, metadata$pin_hash)
  fs::dir_create(path_version)
  fs::file_copy(path, path_version, overwrite = TRUE)
  write_meta(metadata, path_version)

  # Add to list of versions so we know which is most recent
  versions <- c(versions, metadata$pin_hash)
  update_versions(path_pin, versions)

  invisible(board)
}

#' @export
pin_fetch.pins_board_local <- function(board, name, version = NULL, ...) {
  pin_meta(board, name, version = version)
}

#' @export
pin_meta.pins_board_local <- function(board, name, version = NULL, ...) {
  check_name(name)
  path_pin <- fs::path(board$path, name)
  if (!fs::dir_exists(path_pin)) {
    abort(paste0("Can't find pin '", name, "'"))
  }

  version <- version %||%
    last(read_versions(path_pin)) %||%
    abort("No versions found")

  path_version <- fs::path(path_pin, version)
  if (!fs::dir_exists(path_version)) {
    abort(paste0("Can't find version '", version, "'"))
  }

  meta <- read_meta(path_version)
  local_meta(meta, dir = path_version, version = version)
}


# Helpers -----------------------------------------------------------------

read_versions <- function(path) {
  path <- fs::path(path, "versions.yml")
  read_cache(path)$versions
}
update_versions <- function(path, x) {
  path <- fs::path(path, "versions.yml")
  update_cache(path, "versions", x)
}
