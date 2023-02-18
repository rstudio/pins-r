#' Create an automagic board around a single (vanity) url
#'
#' @param url the url
#'
#' @return the board
#' @export
board_auto <- function(url) {
  underlying <- board_and_name_from_url(url)
  new_board_v1("pins_board_auto",
    cache = NA_character_,
    url = url,
    underlying = underlying
  )
}

is.board.auto <- function(x) inherits(x, "pins_board_auto")

#' @export
board_desc.pins_board_auto <- function(board, ...) {
  paste0("AutoBoard from: '", board$url, "'")
}


# Methods -----------------------------------------------------------------

#' @export
pin_list.pins_board_auto <- function(board, ...) {
 board$url
}

board_auto_check_name_is_empty_or_auto <- function(board, name) {
  missing(name) || is.null(name) || name == board$url
}

#' @export
pin_exists.pins_board_auto <- function(board, name, ...) {
  board_auto_check_name_is_empty_or_auto(board, name)
}

#' @export
pin_delete.pins_board_auto <- function(board, names, ...) {
  if (!board_auto_check_name_is_empty_or_auto(names)) {
    abort("different name not expected here!")
  }
  # slightly worried about this... but is OK as long as there's just a single url
  pin_delete(board$underlying$board, board$underlying$name)
  invisible(board)
}

#' @export
pin_store.pins_board_auto <- function(board, name, paths, metadata,
                                              versioned = NULL, ...) {
  check_pin_exists(board, name)
  pin_store(board$underlying$board, board$underlying$name, paths, metadata, versioned = versioned, ...)
  board$underlying$name
}

#' @export
pin_fetch.pins_board_auto <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  pin_fetch(board$underlying$board, board$underlying$name, version = version, ...)
}

#' @export
pin_meta.pins_board_auto <- function(board, name, version = NULL, ...) {
  check_pin_exists(board, name)
  pin_meta(board$underlying$board, board$underlying$name)
}

#' @export
pin_versions.pins_board_auto <- function(board, name, ...) {
  check_pin_exists(board, name)
  pin_versions(board$underlying$board, board$underlying$name, ...)
}

#' @export
pin_version_delete.pins_board_auto <- function(board, name, version, ...) {
  check_pin_exists(board, name)
  pin_version_delete(board$underlying$board, board$underlying$name, version, ...)
}

#' @rdname board_deparse
#' @export
board_deparse.pins_board_auto <- function(board, ...) {
  abort("Not sure what I'm supposed to do here... code to create the board...")
  #path <- check_board_deparse(board, "path")
  #expr(board_folder(path = !!as.character(path)))
}

#' @export
write_board_manifest_yaml.pins_board_auto <- function(board, manifest, ...) {
  abort("Not sure what I'm supposed to do here... Needed for writing new boards")
  # yaml::write_yaml(
  #   manifest,
  #   file = fs::path(board$path, manifest_pin_yaml_filename)
  # )
}
