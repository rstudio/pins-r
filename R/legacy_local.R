#' Local board (legacy API)
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' `legacy_local()` powers `board_register_local()`, which allows you to
#' access local pins created in earlier versions of the pins package. For
#' new pins, we recommend that you transition to [board_local()] which
#' supports the new pins API.
#'
#' `legacy_temp()` creates a legacy board in a temporary location, for use
#' in tests and examples.
#'
#' @inheritParams new_board
#' @param path Path where pins will be stored. If not supplied, defaults
#'   to a system **cache** directory, which may be deleted by the operating
#'   system if you run out of disk space.
#' @export
#' @keywords internal
#' @examplesIf rlang::is_installed("filelock")
#' # Old api
#' pin(data.frame(x = 1:3), "test")
#' pin_get("test")
#'
#' # New api
#' board <- board_local()
#' board %>% pin_write(data.frame(x = 1:3), "test")
#' board %>% pin_read("test")
legacy_local <- function(path = NULL, name = "local", versions = FALSE) {
  path <- path %||% board_cache_path(name)
  fs::dir_create(path)

  new_board_v0("pins_board_local",
    name = name,
    cache = NA,
    path = path,
    versions = versions
  )
}

#' @rdname legacy_local
#' @export
board_register_local <- function(name = "local", cache = NULL, ...) {
  lifecycle::deprecate_soft(
    "1.4.0",
    "board_register_local()",
    details = 'Learn more at <https://pins.rstudio.com/articles/pins-update.html>'
  )
  board <- legacy_local(path = cache, name = name, ...)
  board_register2(board)
}


#' @export
#' @rdname legacy_local
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
