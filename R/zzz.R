.onLoad <- function(libname, pkgname) {
  board_register_local()
}

.globals <- new.env(parent = emptyenv())
.globals$ui_updating_connection <- 0
.globals$boards_registered <- list()
