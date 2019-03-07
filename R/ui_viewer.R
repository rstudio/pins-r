pins_viewer_register <- function(backend = "local") {
  observer <- getOption("connectionObserver")
  if (identical(observer, NULL)) return()

  icons <- system.file(file.path("icons"), package = "pins")

  observer$connectionOpened(
    # connection type
    type = "Pins",

    # name displayed in connection pane
    displayName = backend,

    # host key
    host = backend,

    # connection code
    connectCode = paste0("use_board(\"", backend, "\")"),

    # disconnection code
    disconnect = function() {
      observer$connectionClosed(type = "Pins", host = backend)
    },

    listObjectTypes = function () {
      list(
        table = list(contains = "data")
      )
    },

    # table enumeration code
    listObjects = function(type = "table") {
      objects <- find_pin()
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
      View(iris)
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
    connectionObject = list(name = backend)
  )
}

pins_viewer_updated <- function() {
  viewer <- getOption("connectionObserver")
  if (!is.null(viewer))
    viewer$connectionUpdated(type = "Pins", host = "local", hint = "")
}
