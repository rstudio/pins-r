#' Use a local folder as board
#'
#' @description
#' * `board_folder()` creates a board inside a folder. You can use this to
#'    share files by using a folder on a shared network drive or inside
#'    a DropBox.
#'
#' * `board_temp()` creates a temporary board that lives in a session
#'    specific temporary directory. It will be automatically deleted once
#'    the current R session ends.
#'
#' * `board_local()` is mostly included for backward compatiblity. It's
#'    the default board used by [pin()] and [pin_get()].
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

#' @export
board_desc.pins_board_local <- function(board, ...) {
  paste0("Path: '", board$cache, "'")
}

#' @export
board_browse.pins_board_local <- function(board, ...) {
  utils::browseURL(board$cache)
}

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
board_pin_find.pins_board_local <- function(board, text, ...) {
  results <- pin_registry_find(board, text)

  # if (nrow(results) == 1) {
  #   metadata <- jsonlite::fromJSON(results$metadata)
  #   path <-  pin_registry_absolute(metadata$path, component = board$name)
  #   extended <- pin_manifest_get(path)
  #   merged <- pin_manifest_merge(metadata, extended)
  #
  #   results$metadata <- as.character(jsonlite::toJSON(merged, auto_unbox = TRUE))
  # }

  results
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

#' @export
board_pin_remove.pins_board_local <- function(board, name, ...) {
  pin_registry_remove(board, name)
}

#' @export
board_pin_versions.pins_board_local <- function(board, name, ...) {
  board_versions_get(board, name)
}

# pins 1.0.0 --------------------------------------------------------------

#' @export
board_pin_upload.pins_board_local <- function(board, name, path, metadata,
                                              versioned = NULL, ...) {
  # TODO: backward compatibility layer
  # TODO: what happens if no metadata present? More important for (e.g.)
  #   S3 where other writers might be using different systems

  dest <- fs::path(board$cache, name)
  fs::dir_create(dest)
  meta <- read_meta(dest)

  # board$versioning should default to NULL, but user could choose to turn it on
  versioned <- versioned %||% meta$versioned %||% board$versions %||%
    abort("Must supply `versioned` argument")
  if (isFALSE(versioned) && isTRUE(meta$versioned)) {
    abort(c(
      "Pin is versioned, but you have requested a write without versions",
      i = "To un-version a pin, you must delete it"
    ))
  }

  if (has_name(meta$versions, metadata$file_hash)) {
    # might still want to update metadata even though file hasn't changed
    upload_inform("unchanged", name)
    return()
  } else {
    if (versioned) {
      upload_inform("versioned", name, metadata$file_hash)
    } else {
      if (length(meta) > 0) {
        upload_inform("replace", name)
      } else {
        upload_inform("create", name)
      }
    }
  }

  meta$versions[[metadata$file_hash]] <- metadata
  meta$versioned <- versioned

  write_meta(meta, dest)
  fs::file_copy(path, fs::path(dest, metadata$file_hash))
}

#' @export
board_pin_download.pins_board_local <- function(board, name, version = NULL, ...) {
  dest <- fs::path(board$cache, name)
  meta_all <- read_meta(dest)

  if (is.null(version)) {
    meta <- meta_all$versions[[length(meta_all$versions)]]
  } else {
    if (!has_name(meta_all$versions, version)) {
      abort(paste0("Can't find version ", version))
    }
    meta <- meta_all$versions[[version]]
  }

  list(
    meta = meta,
    path = fs::path(dest, meta$file_hash)
  )
}

read_meta <- function(path) {
  path <- fs::path(path, "meta.yml")

  if (!fs::file_exists(path)) {
    list()
  } else {
    yaml::read_yaml(path, eval.expr = FALSE)
  }
}
write_meta <- function(x, path) {
  path <- fs::path(path, "meta.yml")
  yaml::write_yaml(x, path)
}

board_pin_delete.pins_board_local <- function(board, name, ...) {
  fs::dir_delete(fs::path(board$cache, name))
}

board_pin_list.pins_board_local <- function(board, ...) {
  fs::path_name(fs::dir_ls(board$cache, type = "directory"))
}


