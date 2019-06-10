pin_preview.data.frame <- function(x) {
  head(x, n = getOption("pins.preview", 10^3))
}
