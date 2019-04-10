#' Create Pin
#'
#' Pins the given dataset or object in the active board.
#'
#' @param x The dataset or object to pin.
#' @param name The name for the dataset or object.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#' @param ... Additional parameters.
#'
#' @export
pin <- function(x, name, description = "", board = active_board(), ...) {
  unpin(name, board = board)

  metadata <- jsonlite::toJSON(pin_metadata(x), auto_unbox = TRUE)
  x <- pin_pack(x, board, ...)
  type <- attr(x, "pin_type")
  if (is.null(type))
    stop("Packing a pin requires 'pin_type' attribute to be specified.")

  pin_create(board, x, name, description, type, metadata)

  pins_viewer_updated()

  result <- get_pin(name, board$name)

  pins_viewer_ensure(board)
  result
}

pin_metadata <- function(x) {
  UseMethod("pin_metadata")
}

#' Pin Extension Metadata
#'
#' Creates metadata for a pin when extending boards.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @export
pins_metadata_create <- function(rows, cols) {
  list(
    rows = rows,
    cols = cols
  )
}

pin_metadata.data.frame <- function(x) {
  pins_metadata_create(nrow(x), ncol(x))
}

pin_metadata.default <- function(x) {
  list()
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
get_pin <- function(name, board = NULL, ...) {
  pin_index <- find_pin(name, board = board)
  pin_index <- pin_index[pin_index$name == name,]
  if (nrow(pin_index) == 0) stop("'", name, "' not found.")

  pin_index <- pin_index[1,]
  if (is.null(board)) board <- pin_index$board

  board_object <- get_board(board)

  result <- pin_retrieve(board_object, name)

  class(result) <- c(paste0(pin_index$type, "_pin"), class(result))

  result <- pin_unpack(result, board_object, name, ...)

  attr(result, "pin_name") <- name

  if (is.data.frame(result))
    maybe_tibble(result)
  else
    result
}

pin_pack <- function(x, board, ...) {
  UseMethod("pin_pack")
}

pin_unpack <- function(x, board, name, ...) {
  UseMethod("pin_unpack")
}

pin_pack.default <- function(x, board, ...) {
  x
}

pin_unpack.default <- function(x, board, ...) {
  x
}

pin_create <- function(board, x, name, description, type, metadata) {
  UseMethod("pin_create")
}

pin_retrieve <- function(board, name) {
  UseMethod("pin_retrieve")
}

#' Remove Pin
#'
#' Unpins the given named pin from the active board.
#'
#' @param name The name for the dataset.
#' @param board The board where this pin will be placed.
#'
#' @export
unpin <- function(name, board = active_board()) {
  pins_viewer_ensure(board)

  pin_remove(board, name)

  invisible(name)
}

pin_remove <- function(board, name) {
  UseMethod("pin_remove")
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{use_board()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param ... Additional parameters.
#'
#' @export
find_pin <- function(text = NULL, board = NULL, ...) {
  if (is.null(board) ||
      (is.character(board) && (nchar(board) == 0 || identical(board, "all")))) board <- all_boards()
  metadata <- identical(list(...)$metadata, TRUE)
  type <- if (identical(list(...)$type, "table")) pin_is_table_subtype() else list(...)$type

  all_pins <- data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character())

  for (board_name in board) {
    board_object <- get_board(board_name)

    board_pins <- pin_find(board = board_object, text)
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

  maybe_tibble(all_pins)
}

pin_find <- function(board, text) {
  UseMethod("pin_find")
}

#' Preview Pin
#'
#' Previews a named pin from the active board.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param ... Additional parameters.
#'
#' @export
preview_pin <- function(name, board = NULL, ...) {
  pin_preview(get_pin(name, board = board))
}

pin_preview <- function(x) {
  UseMethod("pin_preview")
}

pin_preview.data.frame <- function(x) {
  x
}

pin_preview.default <- function(x) {
  stop("Preview unsupported for '", class(x)[[1]], "'")
}

pin_is_table_subtype <- function() {
  c(
    "table",
    "dbplyr"
  )
}
