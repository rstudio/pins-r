library(pins)

pins_addin_source_choices <- function() {
}

pins_addin_server <- function(input, output, session) {
  observe({
    results <- find_pin(input$search)
    session$sendCustomMessage("search-results", results)
  })
}

shinyApp(htmlTemplate("www/index.html"), pins_addin_server)
