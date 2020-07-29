# nocov start

.globals$ui_updating_connection <- 0

ui_viewer_register <- function(board, board_call) {
  if (is.null(.globals$ui_connections)) .globals$ui_connections <- list()

  if (identical(.globals$ui_connections[[board$name]], TRUE)) {
    ui_viewer_updated(board)
    return()
  }

  .globals$ui_connections[[board$name]] <- TRUE

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
      board_deregister(board$name, disconnect = FALSE)
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
      .globals$ui_updating_connection <- 0

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

      pin_index <- pin_find(table, board = board$name, name = table, metadata = TRUE)
      pin_index <- pin_index[table == pin_index$name,]

      if (!is.null(pin_index$metadata) || nchar(pin_index$metadata) > 0) {
        metadata <- jsonlite::fromJSON(pin_index$metadata)
        if (!is.null(metadata$columns)) {
          if (is.vector(metadata$columns)) {
            attr_names <- c(attr_names, names(metadata$columns))
            attr_values <- c(attr_values, as.character(metadata$columns))
          }
          else {
            attr_names <- metadata$columns$name
            attr_values <- metadata$columns$type
          }
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
  if (!is.null(viewer) && .globals$ui_updating_connection < as.integer(Sys.time())) {
    pin_log("Updating connections pane")
    .globals$ui_updating_connection <- as.integer(Sys.time()) + 10
    viewer$connectionUpdated(type = "Pins", host = board$name, hint = "")
  }
}

ui_viewer_closed <- function(board) {
  viewer <- getOption("connectionObserver")
  if (!is.null(viewer)) {
    viewer$connectionClosed(type = "Pins", host = board$name)
    .globals$ui_connections[[board$name]] <- FALSE
  }
}

# nocov end

