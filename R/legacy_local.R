
legacy_local <- function(path = NULL, name = "local", versions = FALSE) {
  path <- path %||% rappdirs::user_data_dir("pins")
  fs::dir_create(path)

  new_board("pins_board_local",
    name = name,
    cache = NA_character_,
    path = path,
    versions = versions
  )
}

legacy_temp <- function(name = "temp", ...) {
  legacy_local(tempfile(), name = name, ...)
}

#' @export
pin_list.pins_board_local <- function(board, ...)  {
  # so default print method works
  board_pin_find(board, NULL)$name
}

#' @export
board_browse.pins_board_local <- function(board, ...) {
  utils::browseURL(board$path)
}

#' @export
board_pin_find.pins_board_local <- function(board, text, ...) {
  pin_registry_find(board, text)
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
board_pin_versions.pins_board_local <- function(board, name, ...) {
  board_versions_get(board, name)
}

#' @export
board_pin_remove.pins_board_local <- function(board, name, ...) {
  pin_registry_remove(board, name)
}
