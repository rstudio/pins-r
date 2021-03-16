#' Find Pin
#'
#' Search a board (or boards) for a pin.
#'
#' @param text The text to find in the pin description or name.
#' @param board The board name used to find the pin.
#' @param name The exact name of the pin to match when searching.
#' @param extended Should additional board-specific columns be shown?
#' @param metadata Include pin metadata in results?
#' @param ... Additional parameters.
#'
#' @details
#'
#' `pin_find()` allows you to discover new resources or retrieve
#' pins you've previously created with `pin()`.
#'
#' The `pins` package comes with a CRAN packages board which
#' allows searching all CRAN packages; however, you can add additional
#' boards to search from like Kaggle, Github and RStudio Connect.
#'
#' For 'local' and 'packages' boards, the 'text' parameter searches
#' the title and description of a pin using a regular expression. Other
#' boards search in different ways, most of them are just partial matches,
#' please refer to their documentation to understand how other
#' boards search for pins.
#'
#' Once you find a pin, you can retrieve with `pin_get("pin-name")`.
#'
#' @examples
#' library(pins)
#'
#' # retrieve pins
#' pin_find()
#'
#' # search pins related to 'cars'
#' pin_find("cars")
#' @export
pin_find <- function(text = NULL,
                     board = NULL,
                     name = NULL,
                     extended = FALSE,
                     metadata = FALSE,
                     ...) {
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
  results <- do.call("rbind", results)

  if (!is.null(name)) {
    results <- results[grepl(paste0("(.*/)?", name, "$"), results$name), ]
  }

  if (!metadata) {
    results$metadata <- NULL
  }

  results <- results[order(results$name), ]

  format_tibble(results)
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

pin_split_owner <- function(name) {
  parts <- strsplit(name, "/")[[1]]
  list(
    owner = if (length(parts) > 1) paste(parts[1:length(parts) - 1], collapse = "/") else NULL,
    name = if (length(parts) > 0) parts[length(parts)] else NULL
  )
}

pin_content_name <- function(name) {
  if (is.character(name)) pin_split_owner(name)$name else name
}

pin_content_owner <- function(name) {
  if (is.character(name)) pin_split_owner(name)$owner else NULL
}

pin_results_merge <- function(r1, r2, merge) {
  if (nrow(r1) > 0) {
    col_diff <- setdiff(names(r2), names(r1))
    if (length(col_diff) > 0) r1[, col_diff] <- ""
  }

  if (nrow(r2) > 0) {
    col_diff <- setdiff(names(r1), names(r2))
    if (length(col_diff) > 0) {
      r2[, col_diff] <- ""
      rownames(r2) <- c()
    }
  }

  rbind(r1, r2)
}
