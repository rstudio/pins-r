
# pins [<img src="man/figures/logo.png" align="right" height="139"/>](https://pins.rstudio.com)

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/pins/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/pins/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)
[![Codecov test
coverage](https://codecov.io/gh/rstudio/pins/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/pins?branch=master)

<!-- badges: end -->

The pins package helps you publish data sets, models, and other R
objects, making it easy to share them across projects and with your
colleagues. You can pin objects to a variety of “boards”, including
local folders (to share on a networked drive or with dropbox), RStudio
Connect, Amazon S3, and more.

pins 1.0.0 includes a new API that is designed to be more explicit and
less magical. The legacy API (`pin()`, `pin_get()`, and
`board_register()`) will continue to work, but new features will only be
implemented with the new API, so we encourage you to switch to the
modern API as quickly as possible. Learn more in
`vignettes("pins-legacy")`.

## Installation

``` r
# Install the released version from CRAN:
install.packages("pins")
```

## Usage

To use the pins package, you must first create a pin board. A good place
to start is `board_folder()`, which stores pins in a directory you
specify. Here I’m using `board_temp()` which is a special case of
`board_folder()` that creates a temporary board that is automatically
deleted when your R session ends:

``` r
library(pins)

b <- board_temp()
b
#> Pin board <pins_board_folder>
#> Path: '/tmp/RtmpLDmcej/pins-25a14b82b2b9'
#> Cache size: 0
```

You can store data in that board with `pin_write()`. The first argument
is the object to pin (normally a data frame), and the second argument is
the name you’ll use to later retrieve it:

``` r
b %>% pin_write(head(mtcars), "mtcars")
#> Guessing `type = 'rds'`
#> Creating new version '20210804T195110Z-f8797'
```

As you can see, the data saved as an `.rds` by default, but depending on
what you’re saving and who else you want to read it, you might save it
as a `csv`, `json`, or `arrow` file, by using the `type` argument.

Later, in a different R session, you can retrieve the data with
`pin_read()`:

``` r
b %>% pin_read("mtcars")
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

A folder board is an easy place to start, but the real power of pins
comes when you use board that is shared with multiple people. For
example, with RStudio Connect you can easily make data available to your
whole team:

``` r
b <- board_rsconnect()
b %>% pin_write(tidy_sales_data, "sales-summary")
#> Saving to hadley/sales-summary
```

Then, someone else (or an automated Rmd report!) can read and use it:

``` r
b <- board_rsconnect()
b %>% pin_read("hadley/sales-summary")
```

You can easily control who gets to access the data using the RStudio
Connection permissions pane.

Learn more in `vignette("pins")`.
