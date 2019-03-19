pins_viewer_register <- function(board, board_call) {
  observer <- getOption("connectionObserver")
  if (identical(observer, NULL)) return()

  icons <- system.file(file.path("icons"), package = "pins")

  observer$connectionOpened(
    # connection type
    type = "Pins",

    # name displayed in connection pane
    displayName = board$name,

    # host key
    host = board$name,

    # icon for connection
    icon = file.path(icons, "pins.png"),

    # connection code
    connectCode = board_call,

    # disconnection code
    disconnect = function() {
      .globals$ui_viewer$active[[board$name]] <- FALSE
      observer$connectionClosed(type = "Pins", host = board$name)
    },

    listObjectTypes = function () {
      list(
        table = list(contains = "data")
      )
    },

    # table enumeration code
    listObjects = function(type = "table") {
      objects <- find_pin(board = board)
      data.frame(
        name = objects$name,
        type = rep("table", length(objects$name)),
        stringsAsFactors = FALSE
      )
    },

    # column enumeration code
    listColumns = function(table) {
      # data.frame(
      #   name = c("col1", "col2"),
      #   type = c("character", "numeric"),
      #   stringsAsFactors = FALSE
      # )
    },

    # table preview code
    previewObject = function(rowLimit, table) {
      get_pin(name = table, board = board)
    },

    # other actions that can be executed on this connection
    actions = list(
      "Help" = list(
        icon = file.path(icons, "help.png"),
        callback = function() {
        }
      )
    ),

    # raw connection object
    connectionObject = list(name = board)
  )
}

pins_viewer_updated <- function() {
  viewer <- getOption("connectionObserver")
  if (!is.null(viewer))
    viewer$connectionUpdated(type = "Pins", host = "pins", hint = "")
}

pins_viewer_ensure <- function(board) {
  if (identical(.globals$ui_viewer, NULL)) .globals$ui_viewer <- list(active = list())
  if (!board %in% .globals$ui_viewer$active ||
      identical(.globals$ui_viewer$active[[board$name]], FALSE)) {
    if (identical(board$name, "local")) {
      pins_viewer_register(board, "use_board(name = \"local\")")
    }
  }

  .globals$ui_viewer$active[[board$name]] <- TRUE
}
