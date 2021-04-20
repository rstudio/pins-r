
# pins <img src="man/figures/logo.png" align="right" width="130px"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/pins/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/pins/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)
[![Codecov test
coverage](https://codecov.io/gh/rstudio/pins/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/pins?branch=master)
<!-- badges: end -->

## Overview

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

To begin using the pins package, you must create a board. A good place
to start is the local board, which stores pins in a local directory.
Here I use a temporary board, so that you can run these examples without
messing up the other data you might have pinned:

``` r
library(pins)

b <- board_temp()
b
#> Pin board <pins_board_local>
#> Path: '/tmp/RtmpdoHnpy/pins-35f166d885db'
#> With no pins.
```

Next you need to store some data in that board with `pin_write()`:

``` r
b %>% pin_write(head(mtcars), "mtcars")
#> Guessing `type = 'rds'`
#> Creating new version 'f879756e28a960ae'
```

As you can see, it’s saved as an `.rds` by default, but depending on
what you’re saving and who else you want to read it, you might save it
as a `csv`, `json`, or `arrow` file. Writing the new data to the same
name pin will overwrite, unless you use versioning: see
`vignette("versioning")` for details.

Later, you can retrieve that data with `pin_read()`:

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
can be different people (or automated processes).

For example, if you use RStudio Connect, you can pin data to shared
board:

``` r
b <- board_rsconnect()
b %>% pin_write(tidy_sales_data, "sales-summary")
#> Saving to hadley/sales-summary
```

And then someone else (or an automated Rmd report) can read it:

``` r
b <- board_rsconnect()
b %>% pin_read("hadley/sales-summary")
```

You can easily control who gets to see it using the RStudio Connection
permissions pane.

As well as RStudio connect, you can share your pins:

-   In shared folders: `board_folder()`.
-   On GitHub: `board_github()`.
-   In Microsoft Azure’s storage: `board_azure()`.
-   On Amazon’s S3: `board_s3()`.
