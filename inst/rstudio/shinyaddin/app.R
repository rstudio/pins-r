library(pins)

pins_addin_source_choices <- function() {
}

pins_addin_server <- function(input, output, session) {
  observe({
    results <- find_pin(input$search, board = input$board, type = "table", metadata = TRUE)
    session$sendCustomMessage("search-results", results)
  })

  observe({
    results <- list(
      boards = pins:::all_boards()
    )

    session$sendCustomMessage("initialized", results)
  })

  observe({
    dataset <- input$dataset
    if (is.character(dataset) && nchar(dataset) > 0) {
      rstudioapi::sendToConsole(paste0(
        "View(pins::preview_pin(\"", dataset , "\")", ", \"", dataset , "\")"
      ))

      stopApp(dataset)
    }
  })
}

shinyApp(htmlTemplate("www/index.html"), pins_addin_server)
