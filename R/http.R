http_utils_progress <- function(type = "down") {
  if (identical(getOption("pins.progress", FALSE), TRUE))
    httr::progress(type = type)
  else
    NULL
}
