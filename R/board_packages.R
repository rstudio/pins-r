board_initialize.packages <- function(...) {

}

pin_find.packages <- function(board, text) {
  if (is.null(text)) {
    return(
      data.frame(name = c(), description = c())
    )
  }

  find_names <- grepl(text, crandatasets$dataset)
  find_description <- grepl(text, crandatasets$description)
  package_pins <- crandatasets[find_names | find_description,]

  data.frame(
    name = package_pins$dataset,
    description = package_pins$description
  )
}
