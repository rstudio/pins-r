get_function <- function(name, package) {
  if (length(find.package(package, quiet = TRUE)) == 0) {
    NULL
  }
  else {
    get0(name, envir = asNamespace(package))
  }
}

pins_show_progress <- function(size = 0) {
  identical(getOption("pins.progress", FALSE), size > 10^7)
}
