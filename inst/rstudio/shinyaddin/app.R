library(pins)

pins_addin_source_choices <- function() {
}

pins_addin_server <- function(input, output, session) {
  board <- reactive(if (identical(input$board, "all")) NULL else input$board)

  observe({
    results <- pin_find(input$search, board = board(), metadata = TRUE)
    session$sendCustomMessage("search-results", results)
  })

  observe({
    results <- list(
      boards = lapply(board_list(), function(e) pins::board_get(e))
    )

    session$sendCustomMessage("initialized", results)
  })

  observe({
    dataset <- input$dataset
    if (is.character(dataset) && nchar(dataset) > 0) {
      board_text <- ""
      if (!is.null(board())) {
        board_text <- paste0(", board = \"", board(), "\"")
      }

      rstudioapi::sendToConsole(paste0(
        "View(pins::pin_preview(pins::pin_get(\"", dataset , "\"", board_text, ")))"
      ))

      stopApp(dataset)
    }
  })
}

shinyApp(htmlTemplate("www/index.html"), pins_addin_server)
