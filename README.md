
# pins <a href="https://pins.rstudio.com"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/pins-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/pins-r/actions/workflows/R-CMD-check.yaml)
[![CRAN
Status](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)
[![Codecov test
coverage](https://codecov.io/gh/rstudio/pins-r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/pins-r?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The pins package publishes data, models, and other R objects, making it
easy to share them across projects and with your colleagues. You can pin
objects to a variety of pin *boards*, including folders (to share on a
networked drive or with services like DropBox), Posit Connect, Amazon
S3, Google Cloud Storage, Azure storage, and Microsoft 365 (OneDrive and
SharePoint). Pins can be automatically versioned, making it
straightforward to track changes, re-run analyses on historical data,
and undo mistakes.

pins 1.0.0 includes a new more explicit API and greater support for
versioning. The legacy API (`pin()`, `pin_get()`, and
`board_register()`) will continue to work, but new features will only be
implemented with the new API, so we encourage you to switch to the
modern API as quickly as possible. Learn more in
`vignette("pins-update")`.

You can use pins from Python as well as R. For example, you can use one
language to read a pin created with the other. Learn more about [pins
for Python](https://rstudio.github.io/pins-python/).

## Installation

You can install pins from CRAN with:

``` r
install.packages("pins")
```

You can install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("rstudio/pins-r")
```

## Usage

To use the pins package, you must first create a pin board. A good place
to start is `board_folder()`, which stores pins in a directory you
specify. Here I’ll use a special version of `board_folder()` called
`board_temp()` which creates a temporary board that’s automatically
deleted when your R session ends. This is great for examples, but
obviously you shouldn’t use it for real work!

``` r
library(pins)

board <- board_temp()
board
#> Pin board <pins_board_folder>
#> Path:
#> '/var/folders/hv/hzsmmyk9393_m7q3nscx1slc0000gn/T/Rtmp4Z1lsA/pins-8a413b4b4478'
#> Cache size: 0
```

You can “pin” (save) data to a board with `pin_write()`. It takes three
arguments: the board to pin to, an object, and a name:

``` r
board %>% pin_write(head(mtcars), "mtcars")
#> Guessing `type = 'rds'`
#> Creating new version '20230908T204205Z-a800d'
#> Writing to pin 'mtcars'
```

As you can see, the data saved as an `.rds` by default, but depending on
what you’re saving and who else you want to read it, you might use the
`type` argument to instead save it as a Parquet, Arrow, CSV, or JSON
file.

You can later retrieve the pinned data with `pin_read()`:

``` r
board %>% pin_read("mtcars")
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

A board on your computer is good place to start, but the real power of
pins comes when you use a board that’s shared with multiple people. To
get started, you can use `board_folder()` with a directory on a shared
drive or in dropbox, or if you use [Posit
Connect](https://posit.co/products/enterprise/connect/) you can use
`board_connect()`:

``` r
board <- board_connect()
#> Connecting to Posit Connect 2023.01.0 at <https://colorado.posit.co/rsc>
board %>% pin_write(tidy_sales_data, "sales-summary", type = "rds")
#> Writing to pin 'hadley/sales-summary'
```

Then, someone else (or an automated Rmd report) can read and use your
pin:

``` r
board <- board_connect()
board %>% pin_read("hadley/sales-summary")
```

You can easily control who gets to access the data using the Posit
Connect permissions pane.

The pins package also includes boards that allow you to share data on
services like Amazon’s S3 (`board_s3()`), Azure’s blob storage
(`board_azure()`), and Google Cloud Storage (`board_gcs()`). Learn more
in `vignette("pins")`.
