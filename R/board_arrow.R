arrow_dependencies <- function() {
  if (!"arrow" %in% installed.packages()) stop("Package 'arrow' needs to be installed to use an 'arrow' board.")

  list (
    read = get("read_arrow", envir = asNamespace("arrow")),
    write = get("write_arrow", envir = asNamespace("arrow"))
  )
}

board_initialize.arrow <- function(...) {

}

pin_create.arrow <- function(board, x, name, description, type, metadata) {
  deps <- arrow_dependencies()
  path <- pin_create_yaml(
    name = name,
    description = description,
    type = type,
    metadata = metadata,
    component = "arrow")
  deps$write(x, path)
}

pin_find.arrow <- function(board, text) {
  pin_find_yaml(text, "arrow")
}

pin_retrieve.arrow <- function(board, name) {
  deps <- arrow_dependencies()
  path <- pin_retrieve_yaml(name, "arrow")
  deps$read(path)
}

pin_remove.arrow <- function(board, name) {
  pin_remove_yaml(name, "arrow")
}
