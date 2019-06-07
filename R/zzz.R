.onLoad <- function(libname, pkgname){
  board_register("local", connect = FALSE)
  board_register("packages", connect = FALSE)
  board_register("kaggle", connect = FALSE)
}
