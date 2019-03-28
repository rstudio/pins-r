library(pins)

pins_addin_source_choices <- function() {
}

#' @import rstudioapi
pins_addin_ui <- function() {
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
            display: table-cell;
          }

          .shiny-input-container:nth-child(2) {
            width: 140px;
          }

          .shiny-input-container > div {
            display: inline-block;
          }

          #search {
            width: 300px;
          }

          #board {
            width: 120px;
          }
        ", sep = ""))
      )
    ),
    div(
        style = paste("display: table; width: 100%;"),
        textInput(
          "search",
          "Search:"
        ),
        selectInput(
          "board",
          "Board:",
          choices = c(
            list(
              "All" = "all",
              "Local" = "local",
              "Packages" = "packages"
            ),
            pins_addin_source_choices()
          ),
          selectize = FALSE
        )
    ),
    div(
      style = paste("display: table-row; height: 30px;")
    )
  )
}

pins_addin_server <- function(input, output, session) {

}

shinyApp(pins_addin_ui, pins_addin_server)
