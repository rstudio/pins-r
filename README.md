pins: manage, discover and share datasets in R.
================

[![Build
Status](https://travis-ci.org/javierluraschi/datapins.svg?branch=master)](https://travis-ci.org/javierluraschi/datapins)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/datapins)](https://cran.r-project.org/package=datapins)

  - **Manage** your personal datasets by pinning and retrieving them
    with `pin()`.
  - **Discover** new datasets from R packages, online and in your
    organization using `find_pin()`.
  - **Share** existing **datasets** online and within your organization
    using `publish_pin()`.
  - **Extend** storage locations using **boards** through `use_board()`,
    you decide where your data lives.

## Installation

You can install `datapins` using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("javierluraschi/datapins")
```

## Personal datasets

You can track personal dataset by pinning them as follows:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(pins)

iris %>%
  filter(Sepal.Width < 3, Petal.Width < 1) %>%
  pin("iris-small-width", "A subset of 'iris' with only small widths.")
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          4.4         2.9          1.4         0.2  setosa
    ## 2          4.5         2.3          1.3         0.3  setosa

You can then use this dataset as,

``` r
pin("iris-small-width")
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          4.4         2.9          1.4         0.2  setosa
    ## 2          4.5         2.3          1.3         0.3  setosa

A pin is a tool to help you organize content, not the content itself.
Therefore, you should not use a pin to store your findings; instead, you
should still persist files or check-in reproducible code into GitHub.
Pins are here to help retrieve and find datasets.

The motivation behind pinning is to allow you to easily fetch results
from past data analysis sessions. This can be useful after tidying your
data, since once a dataset is tidy you are likely to reuse this several
times and. You might also have a past analysis in GitHub, but you might
not want to clone, install dependencies and rerun your code just to
access your dataset, that’s another good case for using a pin. Another
use case is to cross-join between datasets to analyse across multiple
projects or help you remember which datasets you’ve used in the past.

You can find previous datasets using `find_pin()`:

``` r
find_pin(board = "local")
```

    ##               name                                description
    ## 1 iris-small-width A subset of 'iris' with only small widths.
