.onLoad <- function(libname, pkgname) {
  board_register2(board_local())
}

.globals <- new.env(parent = emptyenv())
.globals$ui_updating_connection <- 0

