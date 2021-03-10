pins_show_progress <- function(size = 0) {
  if (is.character(size)) size <- as.integer(size)

  large_file <- getOption("pins.progress.size", 10^7)
  identical(getOption("pins.progress", size > large_file), TRUE) && interactive()
}

has_envvars <- function(x) {
  all(Sys.getenv(x) != "")
}
