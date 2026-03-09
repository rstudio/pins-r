# Wrap a pin in a reactive expression

`pin_reactive_read()` and `pin_reactive_download()` wrap the results of
[`pin_read()`](https://pins.rstudio.com/reference/pin_read.md) and
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
into a Shiny reactive. This allows you to use pinned data within your
app, and have the results automatically recompute when the pin is
modified.

## Usage

``` r
pin_reactive_read(board, name, interval = 5000)

pin_reactive_download(board, name, interval = 5000)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/reference/board_url.md) or
  another `board_` function.

- name:

  Pin name.

- interval:

  Approximate number of milliseconds to wait between re-downloading the
  pin metadata to check if anything has changed.

## Examples

``` r
if (FALSE) {
  library(shiny)
  ui <- fluidPage(
    tableOutput("table")
  )

  server <- function(input, output, session) {
    board <- board_local()
    data <- pin_reactive_read(board, "shiny", interval = 1000)
    output$table <- renderTable(data())
  }
  shinyApp(ui, server)
}
```
