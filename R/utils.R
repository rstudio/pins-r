get_function <- function(name, package) {
  if (length(find.package(package, quiet = TRUE)) == 0) {
    NULL
  }
  else {
    get0(name, envir = asNamespace(package))
  }
}
