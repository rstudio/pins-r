http_utils_progress <- function(type = "down", size = 0) {
  if (pins_show_progress(size = size))
    httr::progress(type = type)
  else
    NULL
}
