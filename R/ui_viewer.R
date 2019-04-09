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
        table = list(contains = "data"),
        formula = list(contains = "formula"),
        connection = list(contains = "connection")
      )
    },

    # table enumeration code
    listObjects = function(type = "table") {
      objects <- find_pin(board = board$name)
      types <- objects$type

      types <- gsub("dbplyr", "table", types)

      data.frame(
        name = objects$name,
        type = types,
        stringsAsFactors = FALSE
      )
    },

    # column enumeration code
    listColumns = function(table) {
      attr_names <- c()
      attr_values <- c()

      pin_index <- find_pin(table, board = board$name)
      if (nchar(pin_index$description) > 0) {
        attr_names <- c(attr_names, "description")
        attr_values <- c(attr_values, pin_index$description)
      }

      pin_value <- get_pin(table, board = board$name)
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
      preview_pin(name = table, board = board$name)
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
