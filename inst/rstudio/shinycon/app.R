library(rai)

rai_connection_server_choices <- function() {
}

#' @import rstudioapi
rai_connection_ui <- function() {
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

          .shiny-input-container > .control-label {
            display: table-cell;
            width: 195px;
          }

          .shiny-input-container > div {
            display: table-cell;
            width: 300px;
          }

          #shiny-disconnected-overlay {
            display: none;
          }
        ", sep = ""))
      )
    ),
    div(style = "table-row",
        selectInput(
          "server",
          "Server:",
          choices = c(
            list("local" = "local"),
            rai_connection_server_choices()
          ),
          selectize = FALSE
        )
    ),
    div(
      style = paste("display: table-row; height: 10px")
    )
  )
}

rai_connection_server <- function(input, output, session) {

}

shinyApp(rai_connection_ui, rai_connection_server)
