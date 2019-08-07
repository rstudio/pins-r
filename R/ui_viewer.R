ui_viewer_register <- function(board) {
  if (is.null(.globals$ui_connections)) .globals$ui_connections <- list()

  if (identical(.globals$ui_connections[[board$name]], TRUE)) {
    ui_viewer_updated(board)
    return()
  }

  .globals$ui_connections[[board$name]] <- TRUE

  board_call <- paste0("pins::board_connect(\"", board$name, "\")")
  observer <- getOption("connectionObserver")
  if (identical(observer, NULL)) return()

  icons <- system.file(file.path("icons"), package = "pins")

  actions <- list(
    "Browse" = list(
      icon = file.path(icons, "browse.png"),
      callback = function() {
        board_browse(board)
      }
    ),
    "Help" = list(
      icon = file.path(icons, "help.png"),
      callback = function() {
        utils::browseURL("https://rstudio.github.io/pins/")
      }
    )
  )

  if (identical(class(board), "local")) {
    actions[["Browse"]] <- NULL
  }

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
      observer$connectionClosed(type = "Pins", host = board$name)
      .globals$ui_connections[[board$name]] <- FALSE
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
        name = as.character(names),
        type = rep("table", length(objects$name)),
        stringsAsFactors = FALSE
      )
    },

    # column enumeration code
    listColumns = function(table) {
      attr_names <- c()
      attr_values <- c()

      pin_index <- pin_find(table, board = board$name, extended = TRUE)

      for (name in names(pin_index)) {
        value <- as.character(pin_index[[name]])
        if (length(value) == 1 && nchar(value) > 0 && !identical(value, "NULL")) {
          if (identical(name, "type")) value <- paste0("'", value, "'")

          attr_names <- c(attr_names, name)
          attr_values <- c(attr_values, value)
        }
      }

      data.frame(
        name = attr_names,
        type = attr_values,
        stringsAsFactors = FALSE
      )
    },

    # table preview code
    previewObject = function(rowLimit, table) {
      pin_preview(pin_get(name = table, board = board$name))
    },

    # other actions that can be executed on this connection
    actions = actions,

    # raw connection object
    connectionObject = list(name = board)
  )
}

ui_viewer_updated <- function(board) {
  viewer <- getOption("connectionObserver")
  if (!is.null(viewer))
    viewer$connectionUpdated(type = "Pins", host = board$name, hint = "")
}
