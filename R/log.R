pin_log <- function(...) {
  if (getOption("pins.verbose", FALSE)) {
    message(...)
  }
}
