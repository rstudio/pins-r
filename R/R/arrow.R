as_arrow <- function(x) {
  UseMethod("as_arrow")
}

as_arrow_dependencies <- function() {
  if (!"arrow" %in% installed.packages()) stop("Package 'arrow' needs to be installed to use an 'arrow' board.")

  list (
    write = get("write_arrow", envir = asNamespace("arrow"))
  )
}

as_arrow.data.frame <- function(x) {
  deps <- as_arrow_dependencies

  deps$write(x, raw())
}
