format_tibble <- function(data) {
  if (!is.data.frame(data)) return(data)

  if ("tibble" %in% utils::installed.packages() && !identical(getOption("pins.tibble"), FALSE)) {
    as_tibble <- get("as_tibble", envir = asNamespace("tibble"))
    as_tibble(data)
  }
  else {
    data
  }
}
