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
pin <- function(x, name = NULL, description = "", board = NULL, ...) {
  if (is.null(name)) {
    if (is.character(x)) name <- gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(x)))
    else stop("The 'name' parameter is required for '", class(x)[[1]], "' objects.")
  }

  board <- board_get(board)

  metadata <- as.character(jsonlite::toJSON(pin_metadata(x), auto_unbox = TRUE))

  x <- pin_pack(x, name, board, ...)

  type <- attr(x, "pin_type")
  if (is.null(type)) stop("Packing a pin requires 'pin_type' attribute to be specified.")

  pin_create(board, x, name, description, type, metadata)

  pin_updated(board)

  result <- pin_get(name, board$name)

  pins_viewer_ensure(board)
  result
}

pin_metadata <- function(x) {
  UseMethod("pin_metadata")
}

pin_metadata.data.frame <- function(x) {
  list(row = nrow(x), cols = ncol(x))
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
pin_get <- function(name, board = NULL, ...) {
  board_object <- board_get(board)

  details <- pin_find(name, board = board_object$name, name = name, extended = TRUE)

  if (nrow(details) == 0 && is.null(board)) {
    all_results <- pin_find(name, board = NULL, name = name)
    if (nrow(all_results) == 0) stop("Can't find '", name, "' pin.")

    results_board <- all_results$board[[1]]
    details <- pin_find(name, board = results_board, name = name, extended = TRUE)
    board_object <- board_get(results_board)
  }

  result <- pin_retrieve(board_object, name, details)

  class(result) <- c(paste0(details$type, "_pin"), class(result))

  result <- pin_unpack(result, board_object, name, ...)

  attr(result, "pin_name") <- name

  maybe_tibble(result)
}

pin_pack <- function(x, board, ...) {
  UseMethod("pin_pack")
}

pin_unpack <- function(x, board, name, ...) {
  UseMethod("pin_unpack")
}

pin_pack.default <- function(x, name, board, ...) {
  attr(x, "pin_type") <- "default"
  x
}

pin_unpack.default <- function(x, board, ...) {
  x
}

pin_create <- function(board, x, name, description, type, metadata) {
  UseMethod("pin_create")
}

pin_retrieve <- function(board, name, details) {
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
pin_remove <- function(name, board = NULL) {
  board <- board_get()
  pins_viewer_ensure(board)

  board_remove_pin(board, name)

  invisible(name)
}

board_remove_pin <- function(board, name) {
  UseMethod("board_remove_pin")
}

#' Find Pin
#'
#' Find a pin in any board registered using \code{use_board()} or
#' \code{board_register()}.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param ... Additional parameters.
#'
#' @export
pin_find <- function(text = NULL, board = NULL, ...) {
  if (is.null(board) ||
      (is.character(board) && (nchar(board) == 0 || identical(board, "all")))) board <- board_list()
  metadata <- identical(list(...)$metadata, TRUE)
  type <- if (identical(list(...)$type, "table")) pin_is_table_subtype() else list(...)$type

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
  pin_preview(pin_get(name, board = board))
}

pin_preview_object <- function(x) {
  UseMethod("pin_preview")
}

pin_preview_object.list <- function(x) {
  x
}

pin_preview_object.character <- function(x) {
  x
}

pin_preview_object.data.frame <- function(x) {
  x
}

pin_preview_object.table_pin <- function(x) {
  x
}

pin_preview_object.files_pin <- function(x) {
  x
}

pin_preview_object.default <- function(x) {
  stop("Preview unsupported for '", class(x)[[1]], "'")
}

pin_is_table_subtype <- function() {
  c(
    "table",
    "dbplyr"
  )
}

pin_updated <- function(board) {
  pins_viewer_updated(board)
}

is_file_pin <- function(x) {
  identical(attr(x, "pin_type"), "files")
}
