board_initialize.packages <- function(...) {

}

pin_find.packages <- function(board, text) {
  if (is.null(text)) {
    return(
      data.frame(name = c(), description = c())
    )
  }

  find_packages <- grepl(text, crandatasets$package)
  find_names <- grepl(text, crandatasets$dataset)
  find_description <- grepl(text, crandatasets$description)
  package_pins <- crandatasets[find_packages | find_names | find_description,]

  data.frame(
    name = package_pins$dataset,
    description = paste(gsub(" ?\\.$", "", package_pins$description), "from", package_pins$package, "package.")
  )
}
