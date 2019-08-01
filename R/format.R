format_tibble <- function(data) {
  if (!is.data.frame(data)) return(data)

  if (length(find.package("tibble", quiet = TRUE)) > 0 && !identical(getOption("pins.tibble"), FALSE)) {
    as_tibble <- get("as_tibble", envir = asNamespace("tibble"))
    as_tibble(data)
  }
  else {
    data
  }
}
