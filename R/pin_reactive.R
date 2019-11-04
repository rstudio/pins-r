shiny_dependencies <- function() {
  list(
    reactive_poll = get("reactivePoll", envir = asNamespace("shiny"))
  )
}

pin_changed_time <- function(name, board, extract) {
  pin_path <- board_pin_get(board_get(board), name, extract = extract)
  pin_files <- file.path(pin_path, dir(pin_path))

  max(file.info(pin_files)[,"mtime"])
}

#' Reactive Pin
#'
#' Creates a pin that reacts to changes in the given board by
#' polling \code{pin_get()}, useful when used from the \code{shiny}
#' package.
#'
#' @param name The name of the pin.
#' @param board The board where this pin will be retrieved from.
#' @param interval Approximate number of milliseconds to wait to retrieve
#'   updated pin. This can be a numeric value, or a function that returns
#'   a numeric value.
#' @param session The user session to associate this file reader with, or NULL if
#'   none. If non-null, the reader will automatically stop when the session ends.
#' @param extract Should compressed files be extracted? Each board defines the
#'   deefault behavior.
#'
#' @export
pin_reactive <- function(name, board, interval = 5000, session = NULL, extract = NULL) {
  deps <- shiny_dependencies()

  board_object <- board_get(board)

  deps$reactive_poll(
    intervalMillis = interval,
    session = session,
    checkFunc = function() {
      changed_time <- pin_changed_time(name, board, extract = extract)
      pin_log("pin_reactive() change time: ", changed_time)

      changed_time
    },
    valueFunc = function() {
      pin_get(name, board = board, extract = extract)
    })
}
