# nocov start

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

      pin_index <- pin_find(table, board = board$name, metadata = TRUE)
      pin_index <- pin_index[table == pin_index$name,]

      if (!is.null(pin_index$metadata) || nchar(pin_index$metadata) > 0) {
        metadata <- jsonlite::fromJSON(pin_index$metadata)
        if (!is.null(metadata$columns)) {
          attr_names <- c(attr_names, names(metadata$columns))
          attr_values <- c(attr_values, as.character(metadata$columns))
        }

        if (identical(metadata$type, "files") && length(attr_names) == 0) {
          attr_names <- c(attr_names, "files")
          attr_values <- c(attr_values, "character")
        }
      }

      if (length(attr_names) == 0) {
        attr_names <- c(attr_names, "unknown")
        attr_values <- c(attr_values, "unknown")
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

ui_viewer_closed <- function(board) {
  viewer <- getOption("connectionObserver")
  if (!is.null(viewer))
    viewer$connectionClosed(type = "Pins", host = board$name)
}

# nocov end

