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
board_pin_create <- function(board, path, name, description, type, metadata) {
  UseMethod("board_pin_create")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_initialize <- function(board, ...) {
  UseMethod("board_initialize")
}

board_initialize.default <- function(board, ...) board

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_get <- function(board, name, details) {
  UseMethod("board_pin_get")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_remove <- function(board, name) {
  UseMethod("board_pin_remove")
}

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_find <- function(board, text, ...) {
  UseMethod("board_pin_find")
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

#' @export
#' @rdname custom-boards
#' @keywords internal
board_load <- function(board) {
  UseMethod("board_load")
}

board_load.default <- function(board) board

#' @export
#' @rdname custom-boards
board_persist <- function(board) {
  UseMethod("board_persist")
}

board_persist.default <- function(board) structure(list(board = board$board, name = board$name), class = board$board)

#' @export
#' @rdname custom-boards
#' @keywords internal
board_info <- function(board) {
  UseMethod("board_info")
}

board_info.default = function(board) NULL

#' @export
#' @rdname custom-boards
#' @keywords internal
board_pin_store <- function(board, path, name, description, type, metadata, ...) {
  if (is.null(name)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(path)))

  metadata <- as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE))

  board_pin_create(board, path, name, description, type, metadata, ...)

  pin_get(name, board$name)
}
