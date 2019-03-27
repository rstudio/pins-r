rlang_dependencies <- function() {
  if (!"rlang" %in% installed.packages()) stop("Package 'rlang' needs to be installed to process a formula pin.")

  list (
    as_function = get("as_function", envir = asNamespace("rlang"))
  )
}

pin_pack.formula <- function(x, board, ...) {
  structure(
    paste(as.character(x), collapse = " "),
    class = "formula_pin"
  )
}

pin_unpack.formula_pin <- function(x, board, name, ...) {
  deps <- rlang_dependencies()

  deps$as_function(as.formula(x))()
}
