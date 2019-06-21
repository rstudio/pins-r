#' Pin Resource
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
#' Retrieves a pin by name from the local or given board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param cache Should the pin be cached for remote boards? Defaults to \code{TRUE}.
#' @param ... Additional parameters.
#'
#' @export
pin_get <- function(name, board = NULL, cache = TRUE, ...) {
  if (is.null(board)) {
    board_pin_get_or_null <- function(...) tryCatch(board_pin_get(...), error = function(e) NULL)

    result <- board_pin_get_or_null(board_get(NULL), name)

    if (is.null(result) && is.null(board)) {
      for (board_name in board_list()) {
        result <- board_pin_get_or_null(board_get(board_name), name)
        if (!is.null(result)) break
      }
    }
    if (is.null(result)) stop("Failed to retrieve '", name, "' pin.")
  }
  else if (identical(board, "local") || !cache) {
    result <- board_pin_get(board_get(board), name)
  }
  else {
    result <- pin_get_and_cache(board, name)
  }

  result_type <- pin_type(result)
  if (is.null(result_type)) stop("Pin '", name, "' is missing attribute 'pin_type'")

  result <- pin_load(structure(result, class = result_type))

  attr(result, "pin_type") <- NULL
  attr(result, "pin_metadata") <- NULL

  format_tibble(result)
}

#' Remove Pin
#'
#' Unpins the given named pin from the active board.
#'
#' @param name The name for the pin.
#' @param board The board from where this pin will be removed.
#'
#' @export
pin_remove <- function(name, board) {
  board_pin_remove(board_get(board), name)
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
  if (is.null(board) || nchar(board) == 0) board <- board_list()
  metadata <- identical(list(...)$metadata, TRUE)
  type <- list(...)$type
  text <- pin_content_name(text)

  all_pins <- data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character())

  for (board_name in board) {
    board_object <- board_get(board_name)

    board_pins <- board_pin_find(board = board_object, text, ...)
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

  format_tibble(all_pins)
}

#' Preview Pin
#'
#' Previews a subset of the pin contents, useful to print or display
#' a subset of the pin contents.
#'
#' @param x The pin to preview, retrieved with \code{pin_get()}.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @keywords internal
#' @export
pin_preview <- function(x, board = NULL, ...) {
  UseMethod("pin_preview")
}

#' Load Pin
#'
#' Load a pin from the given file path making use of the pin type.
#'
#' @param path The file to load as a pin.
#' @param ... Additional parameters.
#'
#' @keywords internal
#' @export
pin_load <- function(path, ...) {
  UseMethod("pin_load")
}

pin_type <- function(x) {
  attr(x, "pin_type")
}

pin_type_set <- function(x, type) {
  attr(x, "pin_type") <- type
  x
}
