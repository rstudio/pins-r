#' Create Pin
#'
#' Pins the given resource locally or to the given board.
#'
#' @param x And object, local file or remote URL to pin.
#' @param name The name for the dataset or object.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @export
pin <- function(x, name = NULL, description = NULL, board = NULL, ...) {
  UseMethod("pin")
}

#' Retrieve Pin
#'
#' Retrieves a named pin from the active board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @export
pin_get <- function(name, board = NULL, ...) {

  if (is.null(board)) {
    result <- board_pin_get_or_null(board_get(board_name), name)

    if (is.null(result) && is.null(board)) {
      for (board_name in board_list()) {
        result <- board_pin_get_or_null(board_get(board_name), name)
        if (!is.null(result)) break
      }
    }
    if (is.null(result)) stop("Failed to retrieve '", name, "' pin.")
  }
  else {
    result <- board_pin_get(board_get(board), name)
  }

  maybe_tibble(result)
}

#' Extensible API
#'
#' Family of functions meant to be used to extend pins to support new
#' boards, not to be used by end users.
#'
#' @param x A local file path or an object. Boards must support storing both.
#'
#' @export
#' @keywords internal
board_create_pin <- function(board, x, name, description, type, metadata) {
  on.exit(pins_viewer_updated(board))

  UseMethod("board_create_pin")
}

#' Remove Pin
#'
#' Unpins the given named pin from the active board.
#'
#' @param name The name for the resource.
#' @param board The board where this pin will be placed.
#'
#' @export
pin_remove <- function(name, board = NULL) {
  board <- board_get()

  board_remove_pin(board, name)

  invisible(name)
}

board_remove_pin <- function(board, name) {
  UseMethod("board_remove_pin")
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{board_register()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param ... Additional parameters.
#'
#' @export
pin_find <- function(text = NULL, board = NULL, ...) {
  if (is.null(board)) board <- board_list()
  metadata <- identical(list(...)$metadata, TRUE)
  type <- list(...)$type
  text <- pin_without_owner(text)

  all_pins <- data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character())

  for (board_name in board) {
    board_object <- board_get(board_name)

    board_pins <- board_find_pin(board = board_object, text, ...)
    board_pins$board <- rep(board_name, nrow(board_pins))

    if (!identical(type, NULL)) {
      board_pins <- board_pins[board_pins$type %in% type,]
    }

    all_pins <- rbind(all_pins, board_pins)
  }

  if (!is.null(text)) {
    find_names <- grepl(text, all_pins$name)
    find_description <- grepl(text, all_pins$description)
    all_pins <- all_pins[find_names | find_description,]
  }

  if (!metadata) {
    all_pins <- all_pins[, names(all_pins) != "metadata"]
  }

  if (!is.null(list(...)$name)) {
    name <- list(...)$name
    all_pins <- all_pins[all_pins$name == name,]
    if (nrow(all_pins) > 0) all_pins <- all_pins[1,]
  }

  maybe_tibble(all_pins)
}

board_find_pin <- function(board, text, ...) {
  UseMethod("board_find_pin")
}

#' Preview Pin
#'
#' Previews a named pin from the active board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @keywords internal
#' @export
pin_preview <- function(name, board = NULL, ...) {
  pin_preview_object(pin_get(name, board = board))
}

pin_preview_object <- function(x) {
  UseMethod("pin_preview_object")
}

pin_preview_object.data.frame <- function(x) {
  head(x, n = getOption("pins.preview", 10^3))
}

pin_preview_object.default <- function(x) {
  x
}

is_file_pin <- function(x) {
  identical(attr(x, "pin_type"), "files")
}
