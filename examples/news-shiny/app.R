library(shiny)
library(dplyr)
library(pins)

board_register("rsconnect", server = "https://beta.rstudioconnect.com")

ui <- fluidPage(
    tags$head(tags$style(HTML("pre { white-space: pre-wrap; }"))),
    titlePanel("News from Headlines"),
    fluidPage(
        fluidRow(
            selectInput("newsHeadline", "Headline", c("loading..."), size = 10, selectize = FALSE, width = "100%")
        ),
        fluidRow(
            verbatimTextOutput("newsContent")
        )
    )
)

server <- function(input, output, session) {
    news <- pin_reactive("news-generated", board = "rsconnect")

    observe({
        choices <- pull(news(), title)
        updateSelectInput(session = session, inputId = "newsHeadline", choices = choices, selected = choices[1])
    })

    output$newsContent <- renderText ({
        filter(news(), title == input$newsHeadline) %>%
            pull(generated) %>%
            paste(input$newsHeadline, .) %>%
            gsub("<\\|endoftext\\|>.*", "", .)
    })
}

shinyApp(ui = ui, server = server)
