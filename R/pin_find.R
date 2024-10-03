#' Search for pins (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Search for pins in legacy boards.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param name The exact name of the pin to match when searching.
#' @param extended Should additional board-specific columns be shown?
#' @param metadata Include pin metadata in results?
#' @param ... Additional parameters.
#' @examples
#' pin_find("cars")
#' # ->
#' board <- board_local()
#' board %>% pin_search("cars")
#' @export
#' @keywords internal
pin_find <- function(text = NULL,
                     board = NULL,
                     name = NULL,
                     extended = FALSE,
                     metadata = FALSE,
                     ...) {
  lifecycle::deprecate_soft("1.4.0", "pin_find()", "pin_search()")

  if (is.null(board)) {
    boards <- lapply(board_list(), board_get)
  } else if (is.character(board)) {
    boards <- lapply(board, board_get)
  } else if (is.board(board)) {
    boards <- list(board)
  } else {
    stop("Unsupported input for `board`", call. = FALSE)
  }

  text <- text %||% name

  results <- lapply(boards, function(board) {
    board_pins <- board_pin_find(board, text, name = name, extended = extended, ...)
    board_pins$board <- rep(board$name, nrow(board_pins))
    board_pins
  })
  results[[length(results) + 1]] <- pin_find_empty()

  # Take lowest common denominator of columns
  names <- lapply(results, names)
  names <- Reduce(intersect, names)
  results <- lapply(results, "[", names)
  results <- do.call("rbind", results)

  if (!is.null(name)) {
    results <- results[grepl(paste0("(.*/)?", name, "$"), results$name), ]
  }

  if (!metadata) {
    results$metadata <- NULL
  }

  results[order(results$name), ]
}

pin_find_empty <- function() {
  data.frame(
    name = character(),
    description = character(),
    type = character(),
    metadata = character(),
    board = character(),
    stringsAsFactors = FALSE
  )
}
