http_utils_progress <- function(type = "down") {
  if (pins_show_progress())
    httr::progress(type = type)
  else
    NULL
}
