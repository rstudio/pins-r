# Search for pins

The underlying search method depends on the `board`, but most will
search for text in the pin name and title.

## Usage

``` r
pin_search(board, search = NULL, ...)
```

## Arguments

- board:

  A pin board, created by
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  or another `board_` function.

- search:

  A string to search for in pin name and title. Use `NULL` to return all
  pins.

- ...:

  Additional arguments passed on to methods.

## Value

A data frame that summarises the metadata for each pin. Key attributes
(`name`, `type`, `description`, `created`, and `file_size`) are pulled
out into columns; everything else can be found in the `meta`
list-column.

## Examples

``` r
board <- board_temp()

board |> pin_write(1:5, "x", title = "Some numbers")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165848Z-0aee4'
#> Writing to pin 'x'
board |> pin_write(letters[c(1, 5, 10, 15, 21)], "y", title = "My favourite letters")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165848Z-a7415'
#> Writing to pin 'y'
board |> pin_write(runif(20), "z", title = "Random numbers")
#> Guessing `type = 'rds'`
#> Creating new version '20260313T165848Z-ba168'
#> Writing to pin 'z'

board |> pin_search()
#> # A tibble: 3 × 6
#>   name  type  title            created             file_size meta      
#>   <chr> <chr> <chr>            <dttm>              <fs::byt> <list>    
#> 1 x     rds   Some numbers     2026-03-13 16:58:48        50 <pins_met>
#> 2 y     rds   My favourite le… 2026-03-13 16:58:48        58 <pins_met>
#> 3 z     rds   Random numbers   2026-03-13 16:58:48       166 <pins_met>
board |> pin_search("number")
#> # A tibble: 2 × 6
#>   name  type  title          created               file_size meta      
#>   <chr> <chr> <chr>          <dttm>              <fs::bytes> <list>    
#> 1 x     rds   Some numbers   2026-03-13 16:58:48          50 <pins_met>
#> 2 z     rds   Random numbers 2026-03-13 16:58:48         166 <pins_met>
board |> pin_search("letters")
#> # A tibble: 1 × 6
#>   name  type  title            created             file_size meta      
#>   <chr> <chr> <chr>            <dttm>              <fs::byt> <list>    
#> 1 y     rds   My favourite le… 2026-03-13 16:58:48        58 <pins_met>
```
