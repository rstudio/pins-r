# nocov start

#' Launch Finder Addin
#'
#' Launches the finder addin.
#'
#' @keywords internal
#' @export
ui_addin_pin_find <- function() {
  shinyAppDir <- get("shinyAppDir", envir = asNamespace("shiny"))
  dialogViewer <- get("dialogViewer", envir = asNamespace("shiny"))
  runGadget <- get("runGadget", envir = asNamespace("shiny"))

  app <- shinyAppDir(system.file("rstudio/shinyaddin", package = "pins"))
  runGadget(app, viewer = dialogViewer("Find Pin"))
}

# nocov end
