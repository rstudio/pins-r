.onLoad <- function(libname, pkgname) {
  board_register2(board_local(board_cache_path()))
}

.globals <- new.env(parent = emptyenv())
.globals$ui_updating_connection <- 0
.globals$boards_registered <- list()
