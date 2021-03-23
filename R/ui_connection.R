# nocov start

#' Launch Connection Addin
#'
#' Launches the connection addin.
#'
#' @keywords internal
#' @export
ui_connection_create <- function() {
  shiny::shinyAppDir(system.file("rstudio/shinycon", package = "pins"))
}

# nocov end
