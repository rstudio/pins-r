maybe_tibble <- function(data) {
  if ("tibble" %in% installed.packages() && !identical(getOption("pins.tibble"), FALSE)) {
    as_tibble <- get("as_tibble", envir = asNamespace("tibble"))
    as_tibble(data)
  }
  else {
    data
  }
}
