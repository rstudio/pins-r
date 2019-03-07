#' Dataset Pin
#'
#' Pins the given dataset in the active board or retrieves
#' a named pin from the active board.
#'
#' @param x A dataset to pin or the named pin to retrieve.
#' @param name The name for the dataset.
#' @param description Optional description for this pin.
#' @param path Optional location where this dataset was stored.
#' @param board The board where this pin will be placed.
#'
#' @export
pin <- function(x = NULL, name = NULL, description = "", path = NULL, board = active_board()) {
  if (is.null(name)) {
    config <- structure(list(
        name = x
      ),
      class = board
    )

    pin_retrieve(config)
  }
  else {
    unpin(name)

    config <- structure(list(
        name = name,
        description = description
      ),
      class = board
    )

    pin_create(config, x)

    pins_viewer_updated()

    x
  }
}

pin_create <- function(config, dataset) {
  UseMethod("pin_create")
}

pin_retrieve <- function(config) {
  UseMethod("pin_retrieve")
}

#' Unpin Dataset
#'
#' Unpins the given named pin from the active board.
#'
#' @param name The name for the dataset.
#' @param board The board where this pin will be placed.
#'
#' @export
unpin <- function(name, board = active_board()) {
  config <- structure(list(
      name = name
    ),
    class = board
  )

  pin_remove(config)
}

pin_remove <- function(config) {
  UseMethod("pin_remove")
}

#' Find Pin
#'
#' Find a pin in the active board.
#'
#' @param name The name for the pin
#' @param board The board where this pin will be placed.
#'
#' @export
find_pin <- function(name = NULL, board = active_board()) {
  config <- structure(list(
      name = name
    ),
    class = board
  )

  pin_find(config, name, contains)
}

pin_find <- function(config, name, contains) {
  UseMethod("pin_find")
}
