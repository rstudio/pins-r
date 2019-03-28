library(pins)

pins_addin_source_choices <- function() {
}

pins_addin_server <- function(input, output, session) {

}

shinyApp(htmlTemplate("www/index.html"), pins_addin_server)
