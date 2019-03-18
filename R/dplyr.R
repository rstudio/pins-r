dplyr_dependencies <- function() {
  if (!"dplyr" %in% installed.packages()) stop("Package 'dplyr' needs to be installed to prepare pin.")

  list (
    collect = get("collect", envir = asNamespace("dplyr"))
  )
}

pin_prepare.tbl_sql <- function(x) {
  deps <- dplyr_dependencies()

  deps$collect(x)
}
