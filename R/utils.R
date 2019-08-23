get_function <- function(name, package) {
  if (length(find.package(package, quiet = TRUE)) == 0) {
    NULL
  }
  else {
    get0(name, envir = asNamespace(package))
  }
}

pins_show_progress <- function() {
  identical(getOption("pins.progress", FALSE), TRUE)
}
