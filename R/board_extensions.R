#' Custom Boards
#'
#' Family of functions meant to be used to implement custom boards extensions,
#' not to be used by users.
#'
#' @param board The board to extend, retrieved with \code{board_get()}.
#' @param path The path to store as a pin.
#' @param name The name of the pin.
#' @param metadata A list of metadata associated with this pin.
#' @param text The text patteren to find a pin.
#' @param ... Additional parameteres.
#'
#' @rdname custom-boards
#' @export
board_pin_create <- function(board, path, name, metadata, ...) {
  UseMethod("board_pin_create")
}

#' @export
#' @rdname custom-boards
board_initialize <- function(board, ...) {
  UseMethod("board_initialize")
}

board_initialize.default <- function(board, ...) stop("Board '", board$name, "' is not a valid board.")

#' @export
#' @rdname custom-boards
board_pin_get <- function(board, name, ...) {
  UseMethod("board_pin_get")
}

#' @export
#' @rdname custom-boards
board_pin_remove <- function(board, name, ...) {
  UseMethod("board_pin_remove")
}

#' @export
#' @rdname custom-boards
board_pin_find <- function(board, text, ...) {
  UseMethod("board_pin_find")
}

#' Custom Boards Utilities
#'
#' A set of utilities used when implementing custom boards.
#'
#' @export
#' @rdname custom-boards-utils
#' @keywords internal
board_local_storage <- function(component = "local") {
  paths <- list(
    unix = "~/.pins",
    windows = file.path(Sys.getenv("LOCALAPPDATA"), "pins")
  )

  path <- paths[[.Platform$OS.type]]

  if (!identical(getOption("pins.path"), NULL)) path <- getOption("pins.path")

  component_path <- file.path(path, component)

  if (!dir.exists(component_path)) dir.create(component_path, recursive = TRUE)

  normalizePath(component_path, mustWork = FALSE)
}

#' @export
#' @rdname custom-boards
board_load <- function(board, ...) {
  UseMethod("board_load")
}

board_load.default <- function(board) board

#' @export
#' @rdname custom-boards
board_persist <- function(board, ...) {
  UseMethod("board_persist")
}

board_persist.default <- function(board) structure(list(board = board$board, name = board$name), class = board$board)

#' @export
#' @rdname custom-boards
board_browse <- function(board, ...) {
  UseMethod("board_browse")
}

board_browse.default <- function(board) { invisible(NULL) }

board_empty_results <- function() {
  data.frame(name = c(), description = c(), rows = c(), cols = c(), class = c())
}
