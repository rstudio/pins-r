library(pins)

rsApiUpdateDialog <- function(code) {
  if (exists(".rs.api.updateDialog")) {
    updateDialog <- get(".rs.api.updateDialog")
    updateDialog(code = code)
  }
}

rsConnectServers <- function() {
  as.character(rsconnect::accounts()$server)
}

#' @import rstudioapi
pins_connection_ui <- function() {
  elementSpacing <- if (.Platform$OS.type == "windows") 2 else 7

  tags$div(
    tags$head(
      tags$style(
        HTML(paste("
          body {
            background: none;

            font-family : \"Lucida Sans\", \"DejaVu Sans\", \"Lucida Grande\", \"Segoe UI\", Verdana, Helvetica, sans-serif;
            font-size : 12px;
            -ms-user-select : none;
            -moz-user-select : none;
            -webkit-user-select : none;
            user-select : none;

            margin: 0;
            margin-top: 7px;
          }

          select {
            background: #FFF;
          }

          .shiny-input-container {
            min-width: 100%;
            margin-bottom: ", elementSpacing, "px;
          }

          .shiny-input-container > .control-label,
          .shiny-input-container > label {
            display: table-cell;
            min-width: 195px;
          }

          .shiny-input-container > div {
            display: table-cell;
            width: 300px;
          }

          .shiny-input-container > .input-group {
            width: 100%;
          }

          .shiny-input-container > .input-group > input {
            border: solid 1px #A2A2A2;
            border-radius: 3px;
            padding: 2px;
          }

          .shiny-input-container > .input-group > .input-group-btn {
            margin-right: 6px;
            border: solid 1px #A2A2A2;
            border-radius: 3px;
            padding: 1px 6px 1px 6px;
          }

          .shiny-input-container > .input-group > input[type=\"text\"] {
            width: 218px;
          }

          .shiny-input-container .progress-bar { display: none; }

          #shiny-disconnected-overlay {
            display: none;
          }
        ", sep = ""))
      )
    ),
    div(
      selectInput(
        "board",
        "Board:",
        choices = c(
          list(
            local = "local",
            rstudio = "rstudio",
            kaggle = "kaggle"
          )
        ),
        selectize = FALSE
      ),
      conditionalPanel(
        condition = "input.board == 'rstudio'",
        selectInput(
          "server",
          "Server:",
          list(),
          selectize = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.board == 'kaggle'",
        fileInput(
          "token",
          "Token:"
        )
      )
    ),
    div(
      style = paste("display: table-row; height: 10px")
    )
  )
}

pins_connection_server <- function(input, output, session) {

  observe({
    if (identical(input$board, "rstudio")) {
      updateSelectizeInput(
        session,
        "server",
        choices = rsConnectServers()
      )
    }
  })

  generateCode <- function(board) {
    parameters <- ""

    if (identical(board, "rstudio") && !is.null(input$server)) {
      parameters <- paste(
        ", server = \"", input$server, "\"",
        sep = "")
    }
    else if (identical(board, "kaggle") && !pins:::kaggle_authenticated()) {

    }

    paste(
      "pins::board_connect(",
      "\"", board, "\"",
      parameters,
      ")",
      sep = ""
    )
  }

  codeReactive <- reactive({
    generateCode(input$board)
  })

  observe({
    rsApiUpdateDialog(codeReactive())
  })
}

shinyApp(pins_connection_ui, pins_connection_server)
