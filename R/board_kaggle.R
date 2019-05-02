kaggle_dependencies <- function() {

}

board_initialize.kaggle <- function(...) {

}

pin_create.kaggle <- function(board, x, name, description, type, metadata) {

}

pin_find.kaggle <- function(board, text) {
  data.frame(name = c(), description = c(), type = c(), metadata = c())
}

pin_retrieve.kaggle <- function(board, name) {

}

pin_remove.kaggle <- function(board, name) {

}

board_info.kaggle <- function(board) {
  list(
    install_html = paste(
      "Download token from:",
      "<a href=\"https://www.kaggle.com/me/account\">kaggle.com/me/account</a>"
    )
  )
}
