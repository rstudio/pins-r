#' Reactive Pin (legacy API)
#'
#' `r lifecycle::badge('deprecated')`
#'
#' Creates a pin that reacts to changes in the given board by
#' polling `pin_get()`, useful when used from the `shiny`
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
#' @keywords internal
pin_reactive <- function(name, board, interval = 5000, session = NULL, extract = NULL) {
  lifecycle::deprecate_soft("1.4.0", "pin_reactive()", "pin_reactive_read()")

  board_object <- board_get(board)

  shiny::reactivePoll(
    intervalMillis = interval,
    session = session,
    checkFunc = function() {
      changed_time <- pin_changed_time(name, board, extract = extract)
      pin_log("pin_reactive() change time: ", changed_time)

      changed_time
    },
    valueFunc = function() {
      pin_get(name, board = board, extract = extract)
    }
  )
}

pin_changed_time <- function(name, board, extract) {
  pin_path <- board_pin_get(board_get(board), name, extract = extract)
  pin_files <- file.path(pin_path, dir(pin_path))

  max(file.info(pin_files)[, "mtime"])
}


#' Wrap a pin in a reactive expression
#'
#' `pin_reactive_read()` and `pin_reactive_download()` wrap the results of
#' [pin_read()] and [pin_download()] into a Shiny reactive. This allows you to
#' use pinned data within your app, and have the results automatically
#' recompute when the pin is modified.
#'
#' @param interval Approximate number of milliseconds to wait between
#'   re-downloading the pin metadata to check if anything has changed.
#' @inheritParams pin_read
#' @export
#' @examples
#' if (FALSE) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     tableOutput("table")
#'   )
#'
#'   server <- function(input, output, session) {
#'     board <- board_local()
#'     data <- pin_reactive_read(board, "shiny", interval = 1000)
#'     output$table <- renderTable(data())
#'   }
#'   shinyApp(ui, server)
#' }
pin_reactive_read <- function(board, name, interval = 5000) {
  check_installed("shiny")

  shiny::reactivePoll(
    intervalMillis = interval,
    session = shiny::getDefaultReactiveDomain(),
    checkFunc = function() get_pin_hash(board, name),
    valueFunc = function() pin_read(board, name)
  )
}

#' @export
#' @rdname pin_reactive_read
pin_reactive_download <- function(board, name, interval = 5000) {
  check_installed("shiny")

  shiny::reactivePoll(
    intervalMillis = interval,
    session = shiny::getDefaultReactiveDomain(),
    checkFunc = function() get_pin_hash(board, name),
    valueFunc = function() pin_download(board, name)
  )
}

get_pin_hash <- function(board, name){
  meta <- pin_meta(board, name)
  meta$pin_hash
}
