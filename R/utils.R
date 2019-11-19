get_function <- function(name, package) {
  if (length(find.package(package, quiet = TRUE)) == 0) {
    NULL
  }
  else {
    get0(name, envir = asNamespace(package))
  }
}

pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE)
}
