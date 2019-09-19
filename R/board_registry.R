board_registry_ensure <- function(register = TRUE) {
  if (identical(.globals$boards_registered, NULL)) {
    .globals$boards_registered <- list()
  }
}

board_registry_list <- function() {
  board_registry_ensure()

  names(.globals$boards_registered)
}

board_registry_get <- function(name) {
  board_registry_ensure()

  .globals$boards_registered[[name]]
}

board_registry_set <- function(name, board) {
  board_registry_ensure(FALSE)

  .globals$boards_registered[[name]] <- board
}
