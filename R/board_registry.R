board_registry_ensure <- function() {
  if (identical(.globals$boards_registered, NULL)) .globals$boards_registered <- list()
}

board_registry_config <- function() {
  file.path(board_local_storage(""), "boards.yml")
}

board_registry_load <- function() {
  board_registry_ensure()

  config <- board_registry_config()

  if (!file.exists(config)) board_registry_persist()
  .globals$boards_registered <- yaml::yaml.load_file(config)

  for (board_name in board_registry_list()) {
    board <- board_registry_get(board_name)
    class(board) <- board$board
    board <- board_load(board)

    board_registry_set(board_name, board)
  }
}

board_registry_persist <- function() {
  board_registry_ensure()

  config <- board_registry_config()

  entries <- .globals$boards_registered

  for (board_name in names(entries)) {
    entries[[board_name]] <- board_persist(entries[[board_name]])
  }

  yaml::write_yaml(entries, config)
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

  board_registry_persist()
}
