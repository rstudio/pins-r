# nocov start

#' Launch Connection Addin
#'
#' Launches the connection addin.
#'
#' @keywords internal
#' @export
ui_connection_create <- function() {
  shinyAppDir <- get("shinyAppDir", envir = asNamespace("shiny"))
  shinyAppDir(system.file("rstudio/shinycon", package = "pins"))
}

# nocov end
