
pin_pack.data.frame <- function(x, board, ...) {
  attr(x, "pin_type") <- "table"
  x
}

pin_unpack.table_pin <- function(x, board, name, ...) {
  x
}
