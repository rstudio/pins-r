library(pinboard)

pinboard_addin_source_choices <- function() {
}

#' @import rstudioapi
pinboard_addin_ui <- function() {
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

            margin: 20px;
            background: #FFF;
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
            width: 295px;
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
          "source",
          "Source:",
          choices = c(
            list("dataframe" = "Data Frame"),
            pinboard_addin_source_choices()
          ),
          selectize = FALSE
        )
    ),
    div(
      style = paste("display: table-row; height: 10px")
    )
  )
}

pinboard_addin_server <- function(input, output, session) {

}

shinyApp(pinboard_addin_ui, pinboard_addin_server)
