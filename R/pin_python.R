python_pin_convert.default <- function(x) {
  data.frame(result = as.character(x))
}

python_pin_convert.character <- function(x) {
  data.frame(files = x)
}

python_pin_convert.data.frame <- function(x) {
  x
}

python_pin_convert <- function(x) {
  UseMethod("python_pin_convert")
}

pin_for_python <- function(x) {
  python_pin_convert(x)
}
