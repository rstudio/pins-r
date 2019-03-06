#' @keywords internal
#' @export
pinboard_connection_create <- function() {
  shinyAppDir <- get("shinyAppDir", envir = asNamespace("shiny"))
  shinyAppDir(system.file("rstudio/shinycon", package = "pinboard"))
}
