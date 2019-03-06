pinboard_addin_pin <- function() {
  shinyAppDir <- get("shinyAppDir", envir = asNamespace("shiny"))
  dialogViewer <- get("dialogViewer", envir = asNamespace("shiny"))
  runGadget <- get("runGadget", envir = asNamespace("shiny"))

  app <- shinyAppDir(system.file("rstudio/shinyaddin", package = "pinboard"))
  runGadget(app, viewer = dialogViewer("Pin Dataset"))
}
