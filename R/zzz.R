.onLoad <- function(libname, pkgname){
  board_registry_load()

  board_register("local", connect = FALSE)
  board_register("packages", connect = FALSE)
}
