#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards extensions,
#' not to be used by end users.
#'
#' @param x A local file path or an object. Boards must support storing both.
#'
#' @rdname custom-boards
#' @export
#' @keywords internal
board_create_pin <- function(board, path, name, description, type, metadata) {
  UseMethod("board_create_pin")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_get <- function(board, name, details) {
  UseMethod("board_pin_get")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_remove_pin <- function(board, name) {
  UseMethod("board_remove_pin")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_find_pin <- function(board, text, ...) {
  UseMethod("board_find_pin")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_local_storage <- function(component) {
  paths <- list(
    unix = "~/pins",
    windows = "%LOCALAPPDATA%/pins"
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pins.path"), NULL)) path <- getOption("pins.path")

  component_path <- file.path(path, component)

  if (!dir.exists(component_path)) dir.create(component_path, recursive = TRUE)

  normalizePath(component_path, mustWork = FALSE)
}
