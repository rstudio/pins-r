#' Write board manifest file to board's root directory
#'
#' A board manifest file contains a named list of pins, where the values
#' identify pin-version directories.
#' This can be useful for a board built using, for example,
#' [board_folder()] or [board_s3()], then served as a website,
#' such that others can consume using [board_url()].
#'
#' This function is not supported for read-only boards.
#' It is called for the side-effect of writing a manifest file,
#' `pins.txt`, to the root directory of the  `board`.
#'
#' @param board A pin board that is *not* read-only.
#' @inheritParams pin_read
#'
#' @return The board, invisibly
#' @export
#'
#' @examples
#' board <- board_temp()
#' pin_write(board, mtcars, "mtcars-csv", type = "csv")
#' pin_write(board, mtcars, "mtcars-json", type = "json")
#'
#' board_manifest(board)
#'
#' # see the manifest's format:
#' fs::path(board$path, "pins.txt") %>% readLines() %>% cat(sep = "\n")
#'
board_manifest <- function(board, ...) {
  manifest <- make_manifest(board)
  write_manifest_yaml(board, manifest, ...)
  pins_inform("Manifest file written to root folder of board, as `pins.txt`")
}

make_manifest <- function(board) {
  # given board, return named list:
  #   - names are pin names
  #   - values are relative paths to version directories

  pin_names <- pin_list(board)

  result <- map(
    pin_names,
    ~fs::path(.x, pin_versions(board, name = .x)$version) %>%
      append_slash() %>%
      as.list()
  )
  names(result) <- pin_names

  result
}

append_slash <- function(x) {
  if (!grepl("/$", x)) {
    x <- paste0(x, "/")
  }
  x
}

#' Write a manifest YAML file for a board
#'
#' This is a low-level function that powers [board_manifest()]. It is needed
#' primarily for folks developing new board types, and should not generally
#' be called directly.
#'
#' @return `write_manifest_yaml()` is called for its side-effect of writing a
#' manifest YAML file. It invisibly returns the board.
#' @export
#' @keywords internal
#' @inheritParams board_manifest
#' @param manifest Contents to be written to the manifest file, as a list.
write_manifest_yaml <- function(board, manifest, ...) {
  ellipsis::check_dots_used()
  UseMethod("write_manifest_yaml")
}

#' @export
write_manifest_yaml.default <- function(board, manifest, ...) {
  abort(glue::glue("Manifest not supported for {class(board)[[1]]}"))
}
