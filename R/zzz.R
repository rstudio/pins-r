.onLoad <- function(libname, pkgname) {
  # When testing from R CMD check, don't write into standard config directories
  if (is_rcmd_check()) {
    if (identical(Sys.getenv("R_USER_CACHE_DIR"), "")) {
      Sys.setenv(R_USER_CACHE_DIR = tempfile())
    }
    if (identical(Sys.getenv("R_USER_DATA_DIR"), "")) {
      Sys.setenv(R_USER_DATA_DIR = tempfile())
    }
  }

  board_register_local()
}

.globals <- new.env(parent = emptyenv())
.globals$ui_updating_connection <- 0
.globals$boards_registered <- list()
