board_registry_ensure <- function() {
  if (identical(.globals$boards_registered, NULL)) {
    .globals$boards_registered <- list()
    board_registry_defaults()
  }
}

board_registry_defaults <- function() {
  if (!"local" %in% names(.globals$boards_registered)) board_register("local", name = "local", connect = FALSE)
  if (!"packages" %in% names(.globals$boards_registered)) board_register("packages", cache = dirname(board_local_storage("local", board = board_get("local"))), connect = FALSE)
  if (!board_default() %in% names(.globals$boards_registered)) board_register(board_default(), connect = FALSE)
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
  board_registry_ensure()

  .globals$boards_registered[[name]] <- board
}
