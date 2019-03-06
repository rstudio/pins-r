#' Pin Dataset
#'
#' Pins the given dataset in the active board.
#'
#' @param dataset The dataset to pin.
#' @param name The name for the dataset.
#' @param description Optional description for this pin.
#' @param board The board where this pin will be placed.
#'
#' @export
pin <- function(dataset = NULL, name, description = "", board = active_board()) {
  config <- structure(list(
      name = name,
      description = description
    ),
    class = board
  )

  pin_create(config, dataset)

  pinboard_viewer_updated()

  dataset
}

pin_create <- function(config, dataset) {
  UseMethod("pin_create")
}

unpin <- function(name) {

}

#' Find Pins
#'
#' Find pins in the active board.
#'
#' @param name The name for the pin
#' @param board The board where this pin will be placed.
#'
#' @export
find_pins <- function(name = NULL, board = active_board()) {
  config <- structure(list(
      name = name
    ),
    class = board
  )

  pin_list(config, name, contains)
}

pin_list <- function(config, name, contains) {
  UseMethod("pin_list")
}
