library(pins)

pins_addin_source_choices <- function() {
}

pins_addin_server <- function(input, output, session) {
  observe({
    search <- input$search
    session$sendCustomMessage("search-results", search)
  })
}

shinyApp(htmlTemplate("www/index.html"), pins_addin_server)
