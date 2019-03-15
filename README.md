pins: manage, discover and share datasets in R.
================

[![Build
Status](https://travis-ci.org/javierluraschi/pins.svg?branch=master)](https://travis-ci.org/javierluraschi/pins)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)

  - **Manage** your personal datasets by pinning and retrieving them
    with `pin()`.
  - **Discover** new datasets from R packages, online and in your
    organization using `find_pin()`.
  - **Share** existing **datasets** online and within your organization
    using `publish_pin()`.
  - **Extend** storage locations using **boards** through `use_board()`,
    you decide where your data lives.

## Installation

You can install `pins` using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("javierluraschi/pins")
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
find_pin()
```

    ##               name                                description
    ## 1             iris                 The entire 'iris' dataset.
    ## 2 iris-small-width A subset of 'iris' with only small widths.

## Shared datasets

`pins` supports shared storage locations using boards. A board is a
remote location for you to share datasets with your team privately, or
with the world, publicly. Use `use_board()` to choose a board, currently
only databases are supported; however, `pins` provide an extensible API
you can use to store pins anywhere.

### Databases

First, you will need to create a database connection using `DBI`, it is
outside the scope of this tutorial to teach you how to connect to a
database using `DBI`; however, there are many resources available
online.

For instance, you can use a `bigrquery` connection to define our
database-backed
board,

``` r
use_board("database", DBI::dbConnect(bigrquery::bigquery(), project = bq_project, dataset = bq_dataset))
```

    ## NULL

Which we can also use to pin a
    dataset,

``` r
pin(iris, "iris", "The entire 'iris' dataset.")
```

    ## Warning in class(obj) <- c("scalar", class(obj)): Setting class(x) to
    ## multiple strings ("scalar", "SQL", ...); result will no longer be an S4
    ## object

    ## Warning: Do not rely on the default value of the row.names argument for
    ## sqlAppendTable(), it will change in the future.

    ## Warning in class(obj) <- c("scalar", class(obj)): Setting class(x) to
    ## multiple strings ("scalar", "SQL", ...); result will no longer be an S4
    ## object

    ## # A tibble: 150 x 5
    ##    Species    Petal_Width Petal_Length Sepal_Width Sepal_Length
    ##    <chr>            <dbl>        <dbl>       <dbl>        <dbl>
    ##  1 versicolor         1.1          3           2.5          5.1
    ##  2 versicolor         1            3.5         2            5  
    ##  3 versicolor         1            3.5         2.6          5.7
    ##  4 versicolor         1            4           2.2          6  
    ##  5 versicolor         1.2          4           2.6          5.8
    ##  6 versicolor         1.3          4           2.3          5.5
    ##  7 versicolor         1.3          4           2.8          6.1
    ##  8 versicolor         1.3          4           2.5          5.5
    ##  9 versicolor         1.5          4.5         3.2          6.4
    ## 10 versicolor         1.5          4.5         3            5.6
    ## # … with 140 more rows

Find pins,

``` r
find_pin()
```

    ##   name                description
    ## 1 iris The entire 'iris' dataset.

And retrieve datasets,

``` r
pin("iris")
```

    ## # A tibble: 150 x 5
    ##    Species    Petal_Width Petal_Length Sepal_Width Sepal_Length
    ##    <chr>            <dbl>        <dbl>       <dbl>        <dbl>
    ##  1 versicolor         1.1          3           2.5          5.1
    ##  2 versicolor         1            3.5         2            5  
    ##  3 versicolor         1            3.5         2.6          5.7
    ##  4 versicolor         1            4           2.2          6  
    ##  5 versicolor         1.2          4           2.6          5.8
    ##  6 versicolor         1.3          4           2.3          5.5
    ##  7 versicolor         1.3          4           2.8          6.1
    ##  8 versicolor         1.3          4           2.5          5.5
    ##  9 versicolor         1.5          4.5         3.2          6.4
    ## 10 versicolor         1.5          4.5         3            5.6
    ## # … with 140 more rows
