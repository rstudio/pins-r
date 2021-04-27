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
  new_board("pins_board_local",
    name = name,
    cache = path,
    versions = versions
  )
}

#' @export
#' @rdname board_folder
board_local <- function(versions = FALSE) {
  board_folder(board_cache_path("local"), name = "local", versions = versions)
}

#' @rdname board_folder
#' @export
board_temp <- function(name = "temp", versions = FALSE) {
  board_folder(fs::file_temp("pins-"), name = name, versions = versions)
}

# Methods -----------------------------------------------------------------

#' @export
board_desc.pins_board_local <- function(board, ...) {
  paste0("Path: '", board$cache, "'")
}

#' @export
board_browse.pins_board_local <- function(board, ...) {
  utils::browseURL(board$cache)
}

#' @export
pin_list.pins_board_local <- function(board, ...) {
  fs::path_file(fs::dir_ls(board$cache, type = "directory"))
}

#' @export
board_pin_find.pins_board_local <- function(board, text, ...) {
  pin_registry_find(board, text)
}

#' @export
board_pin_remove.pins_board_local <- function(board, name, ...) {
  check_name(name)

  fs::dir_delete(fs::path(board$cache, name))
}

#' @export
board_pin_versions.pins_board_local <- function(board, name, ...) {
  path_pin <- fs::path(board$cache, name)
  pin_meta <- read_meta(path_pin)
  versions <- pin_meta$versions %||% character()

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
  browse_url(meta$pin_path)
}

# pins v1 ----------------------------------------------------------------

#' @export
board_pin_upload.pins_board_local <- function(board, name, path, metadata,
                                              versioned = NULL, ...) {
  check_name(name)
  path_pin <- fs::path(board$cache, name)
  pin_meta <- read_meta(path_pin)

  if (length(pin_meta$versions) > 1) {
    versioned <- versioned %||% TRUE
  } else {
    versioned <- versioned %||% board$versions
  }

  if (!versioned) {
    if (length(pin_meta$versions) == 0) {
      pins_inform(paste0("Creating new version '", metadata$pin_hash, "'"))
    } else if (length(pin_meta$versions) == 1) {
      pins_inform(paste0(
        "Replacing version '", pin_meta$versions, "'",
        " with '", metadata$pin_hash, "'"
      ))
      fs::dir_delete(fs::path(path_pin, pin_meta$versions))
      pin_meta$versions <- NULL
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
  pin_meta$versions <- c(pin_meta$versions, metadata$pin_hash)
  write_meta(pin_meta, path_pin)
}

#' @export
pin_cache.pins_board_local <- function(board, name, version = NULL, ...) {
  check_name(name)
  meta <- pin_meta(board, name, version = version)

  if (meta$api_version == 0) {
    path <- board_pin_get(board, name, ...)
    meta$cache_dir <- path
    meta$cache_paths <- fs::path(path, meta$path)
  } else {
    path_version <- fs::path(board$cache, name, meta$version)
    meta$cache_dir <- path_version
    meta$cache_paths <- fs::path(path_version, meta$file)
  }

  meta
}

#' @export
pin_meta.pins_board_local <- function(board, name, version = NULL, ...) {
  path_pin <- fs::path(board$cache, name)
  if (!fs::dir_exists(path_pin)) {
    abort(paste0("Can't find pin '", name, "'"))
  }

  # Fallback to old structure
  meta_pin <- read_meta(path_pin)
  if (meta_pin$api_version == 0) {
    meta <- pin_registry_retrieve(board, name)
    meta$api_version <- 0L
    new_meta(meta)
  } else {
    version <- version %||% last(meta_pin$versions) %||% abort("No versions found")
    path_version <- fs::path(board$cache, name, version)

    if (!fs::dir_exists(path_version)) {
      abort(paste0("Can't find version '", version, "'"))
    }
    meta <- read_meta(path_version)
    meta$version <- version
    meta$pin_path <- path_version
    new_meta(meta)
  }
}

board_pin_delete.pins_board_local <- function(board, name, ...) {
  fs::dir_delete(fs::path(board$cache, name))
}

board_pin_list.pins_board_local <- function(board, ...) {
  fs::path_file(fs::dir_ls(board$cache, type = "directory"))
}


# v0 ----------------------------------------------------------------------

#' @export
board_pin_create.pins_board_local <- function(board, path, name, metadata, ...) {
  board_versions_create(board, name, path)

  cache_path <- pin_registry_path(board, name)

  if (fs::dir_exists(cache_path)) {
    delete <- fs::dir_ls(cache_path)
    delete <- delete[!grepl("(/|\\\\)_versions$", delete)]
    unlink(delete, recursive = TRUE)
  } else {
    fs::dir_create(cache_path)
  }

  file.copy(fs::dir_ls(path), cache_path, recursive = TRUE)

  metadata$path <- name
  pin_registry_update(board, name, metadata)
}

#' @export
board_pin_get.pins_board_local <- function(board, name, version = NULL, ...) {
  rel_path <- pin_registry_retrieve(board, name)$path
  path <- pin_registry_path(board, rel_path)

  if (!is.null(version)) {
    manifest <- pin_manifest_get(path)
    if (!version %in% manifest$versions) {
      version <- board_versions_expand(manifest$versions, version)
    }

    path <- fs::path(path, version)
  }

  path
}

