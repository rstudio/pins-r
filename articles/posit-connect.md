# Posit Connect

This vignette demonstrates how you might use pins and [Posit
Connect](https://posit.co/products/enterprise/connect/) together to
automate ETL for a Shiny app. Pulling expensive data manipulation out of
your Shiny app is a great way to [improve
performance](https://mastering-shiny.org/performance.html#schedule-data-munging),
and pins makes it easy to schedule data updates in a way that
automatically flows into your app.

``` r
library(pins)
```

## Sharing tidied data

Imagine you’re interested in what’s going on in the news, so you’ve
written a little code to scrape the BBC world news feed and turn it into
a tidy dataset:

``` r
library(xml2)

if (interactive()) {
  xml <- read_xml("http://feeds.bbci.co.uk/news/rss.xml")  
} else {
  # Read a saved version of the data to keep this vignette reproducible
  xml <- read_xml("bbc-news.xml")  
}

items <- xml |> xml_find_all("//item")

bbc_news <- tibble::tibble(
  title = items |> xml_find_first("./title") |> xml_text(),
  date = items |> xml_find_first("./pubDate") |> xml_text(),
  url = items |> xml_find_first("./guid") |> xml_text()
)
bbc_news
#> # A tibble: 71 × 3
#>    title                                                    date  url  
#>    <chr>                                                    <chr> <chr>
#>  1 Diana interview: PM concerned after inquiry into BBC de… Fri,… http…
#>  2 Covid in Scotland: Glasgow to be only Scottish area in … Fri,… http…
#>  3 Prince Harry says heavy drinking masked pain of mum Dia… Fri,… http…
#>  4 Coronavirus: Spain to lift restrictions for UK and Japa… Fri,… http…
#>  5 Diana interview: Whistleblower wants apology from BBC b… Fri,… http…
#>  6 Mayhill: Swansea rioters face robust action, police vow  Fri,… http…
#>  7 UK to offer Australia tariff-free trade deal despite fa… Fri,… http…
#>  8 Leonard Blavatnik named UK's richest person with £23bn … Fri,… http…
#>  9 Lady Gaga had a 'psychotic break' after sexual assault … Fri,… http…
#> 10 Big cats seized from park belonging to Tiger King couple Fri,… http…
#> # ℹ 61 more rows
```

Wouldn’t it be nice to spare your colleagues the work of reproducing
this data, and provide them directly with the tidy tibble? That’s easy
to do with pins:

``` r
board <- board_connect()
board |> pin_write(bbc_news)
```

If you open the pin on RSC (e.g. by running
`board |> pin_browse("news_total")`), you’ll see some metadata about the
pin, a preview of the data, and a control panel which allows you to
control who can see the data. Assuming you’ve given your colleagues
access, they can now easily pull down the data for their own analysis:

``` r
board <- board_connect()
board |> pin_read("your_name/bbc_news")
```

## Automating

Here the underlying data will be changing regularly, but the pinned data
will only change when you run
[`pin_write()`](https://pins.rstudio.com/reference/pin_read.md).
Wouldn’t it be great if we could automate that process? One way to do so
is with RSC’s [scheduled
reports](https://docs.posit.co/connect/user/scheduling/).

First, take your code and put it in an Rmd:

    ---
    title: BBC news
    ---

    ```{r}
    library(pins)
    library(xml2)

    xml <- read_xml("http://feeds.bbci.co.uk/news/rss.xml")
    items <- xml |> xml_find_all("//item")

    bbc_news <- tibble::tibble(
      title = items |> xml_find_first("./title") |> xml_text(),
      date = items |> xml_find_first("./pubDate") |> xml_text(),
      url = items |> xml_find_first("./guid") |> xml_text()
    )

    board <- board_connect()
    board |> pin_write(bbc_news)
    ```

    ```{r}
    bbc_news
    ```

Then publish it to Posit Connect, and schedule it to run as often as you
like. Assuming that you have Posit Connect 1.9.0 or later you don’t need
to provide any arguments to
[`board_connect()`](https://pins.rstudio.com/reference/board_connect.md);
pins will automatically publish to the same Connect instance that’s
running the report.

## Shiny apps

If you’re using automatically updated data in a
[Shiny](https://shiny.posit.co/) app, you can use
[`pin_reactive_read()`](https://pins.rstudio.com/reference/pin_reactive_read.md)
to create a reactive dependency so that your app will automatically
update shortly after the data changes:

``` r
library(shiny)
library(pins)

board <- board_connect()

ui <- fluidPage(
  titlePanel("News from the BBC"),
  htmlOutput("news")
)

server <- function(input, output, session) {
  news <- board |> pin_reactive_read("hadley/bbc_news")
  
  output$news <- renderUI({
    title <- htmltools::htmlEscape(news()$title)
    links <- paste0("<a href='", news()$url, "'>", title, "</a>")
    bullets <- paste0("  <li>", links, "</li>", collapse = "\n")
    HTML(paste0("<ul>", bullets, "</ul>"))
  })
}

shinyApp(ui, server)
```

If you deploy this app and watch it while your scheduled reported runs,
you’ll see the data update automatically.
