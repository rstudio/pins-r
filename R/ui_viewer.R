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
      objects <- pin_find(board = board$name)

      names <- objects$name

      data.frame(
        name = names,
        type = rep("table", length(objects$name)),
        stringsAsFactors = FALSE
      )
    },

    # column enumeration code
    listColumns = function(table) {
      attr_names <- c()
      attr_values <- c()

      pin_index <- pin_find(table, board = board$name)
      if (nchar(pin_index$description) > 0) {
        attr_names <- c(attr_names, "description")
        attr_values <- c(attr_values, pin_index$description)
      }

      pin_value <- pin_get(table, board = board$name)
      attr_names <- c(attr_names, "type")
      if (class(pin_value)[[1]] %in% c("tbl_df", "data.frame")) {
        attr_values <- c(attr_values, "data.frame")
      }
      else {
        attr_values <- c(attr_values, class(pin_value)[[1]])
      }

      data.frame(
        name = attr_names,
        type = attr_values,
        stringsAsFactors = FALSE
      )
    },

    # table preview code
    previewObject = function(rowLimit, table) {
      pin_preview(name = table, board = board$name)
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

pins_viewer_updated <- function(board) {
  pins_viewer_ensure(board)

  viewer <- getOption("connectionObserver")
  if (!is.null(viewer))
    viewer$connectionUpdated(type = "Pins", host = board$name, hint = "")
}

pins_viewer_ensure <- function(board) {
  if (identical(.globals$ui_viewer, NULL)) .globals$ui_viewer <- list(active = list())
  if (!board %in% names(.globals$ui_viewer$active) ||
      identical(.globals$ui_viewer$active[[board$name]], FALSE)) {
    if (identical(board$name, "local")) {
      pins_viewer_register(board, paste0("pins::use_board(name = \"", board$name, "\")"))
    }
  }

  .globals$ui_viewer$active[[board$name]] <- TRUE
}
