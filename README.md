
# pins <a href='https://pins.rstudio.com'><img src="man/figures/logo.png" align="right" height="139"/></a>

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
connect, Amazon S3, and more.

## Installation

``` r
# Install the released version from CRAN:
install.packages("pins")
```

## Usage

To use the pins package, you must first create a pin board. A good place
to start is `board_folder()` which stores pins in a directory you
specific. Here I’ll use `board_temp()` which because it creates a
temporary board that will evaporate when your R session ends:

``` r
library(pins)

b <- board_temp()
b
#> Pin board <pins_board_folder>
#> Path: '/tmp/Rtmp6kAi3y/pins-c0083ff0cffe'
#> With no pins.
```

Next you can to store some data in that board with `pin_write()`. The
first argument is the object to pin (normally a data frame) , and the
second argument is the name you’ll use to later retrieve it:

``` r
b %>% pin_write(head(mtcars), "mtcars")
#> Guessing `type = 'rds'`
#> Creating new version 'f879756e28a960ae'
```

As you can see, it’s saved as an `.rds` by default, but depending on
what you’re saving and who else you want to read it, you might save it
as a `csv`, `json`, or `arrow` file.

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

This can be convenient when working locally, but the real power of pins
comes when you use a shared board, because because the writer and reader
can be different people (or automated processes). For example, you can
RStudio Connect you can pin data to board that’s accessible to your
team:

``` r
b <- board_rsconnect()
b %>% pin_write(tidy_sales_data, "sales-summary")
#> Saving to hadley/sales-summary
```

Then someone else (or an automated Rmd report) can read it:

``` r
b <- board_rsconnect()
b %>% pin_read("hadley/sales-summary")
```

You can easily control who gets to access the data using the RStudio
Connection permissions pane.

Learn more in `vignette("pins")`.

## Legacy API

If you’ve used pins in the past, you might be familiar with a somewhat
different API where you first register a board and then refer to it by
name in `pin()` and `pin_get()`:

``` r
board_register_local("example", tempfile())

pin(head(mtcars), "mtcars", board = "example")
pin_get("mtcars", board = "example")
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

You can continue to use this API for the foreseeable future, but where
possible we recommend upgrading to a modern board that uses the newer
API where the board **object** is always the first argument.

It’s also possible to use `pin()` and `pin_get()` without an explicit
board argument, in which case it uses legacy local board:

``` r
pin(data.frame(x = 1:3), "test-data")
pin_get("test-data")
#>   x
#> 1 1
#> 2 2
#> 3 3
```

This board is called `legacy_local()`. It behaves similarly to
`board_local()`, but is completely distinct (i.e. pinned data is not
shared between the boards).

Currently there are four boards that work with the modern API
(`board_local()`, `board_rsconnect()`, `board_s3()`, and `board_url()`)
and eight boards that work with the legacy API
(`board_register_azure()`, `board_register_datatxt()`,
`board_register_dospace()`, `board_register_gcloud()`,
`board_register_github()`, `board_register_kaggle()`,
`board_register_rsconnect()`, `board_register_s3()`) (Note that
`board_rsconnect()` supports both modern and legacy APIs). The set of
boards that supports the modern API will continue to grow in future
releases.
