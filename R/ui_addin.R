# nocov start

#' Launch Finder Addin
#'
#' Launches the finder addin.
#'
#' @keywords internal
#' @export
ui_addin_pin_find <- function() {
  app <- shiny::shinyAppDir(system.file("rstudio/shinyaddin", package = "pins"))
  shiny::runGadget(app, viewer = shiny::dialogViewer("Find Pin"))
}

# nocov end
