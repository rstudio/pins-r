pin_without_owner <- function(name) {
  if (is.character(name)) gsub(".*/", "", name) else name
}
