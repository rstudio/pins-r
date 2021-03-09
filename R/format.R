format_tibble <- function(data) {
  if (!is.data.frame(data)) return(data)

  if (length(find.package("tibble", quiet = TRUE)) > 0 && !identical(getOption("pins.tibble"), FALSE)) {
    tibble::as_tibble(data)
  }
  else {
    data
  }
}
