pins: Track, Discover and Share Datasets
================

[![Build
Status](https://travis-ci.org/javierluraschi/pins.svg?branch=master)](https://travis-ci.org/javierluraschi/pins)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)
![](https://img.shields.io/badge/lifecycle-experimental-red.svg)

  - **Track** local and remote datasets using `pin()` and `get_pin()`.
  - **Discover** new datasets across different boards using
    `find_pin()`.
  - **Share** datasets with your team, or the world with `use_board()`.
  - **Pins** are also available with RStudio, Kaggle and databases
    boards.

## Installation

You can install `pins` using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("rstudio/pins")
```

## Track

`pins` allows you to track local datasets using `pin()` and `get_pin()`
as well as remote datasets. Datasets are tracked through a pin, a tool
to help you track datasets without worrying too much of where the actual
dataset lives. A pin can be either tabular data or arbitrary files.

Lets take a look at using pins with local datasets first.

### Local Datasets

You can track your datasets privately by pinning them with `pin()`.

``` r
library(pins)

head(iris, 2) %>%
  pin("iris-head", "A subset of 'iris' with only small widths.")
```

    ## # A tibble: 2 x 5
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ##          <dbl>       <dbl>        <dbl>       <dbl> <fct>  
    ## 1          5.1         3.5          1.4         0.2 setosa 
    ## 2          4.9         3            1.4         0.2 setosa

You can then retrieve them back with `get_pin()`.

``` r
get_pin("iris-head")
```

    ## # A tibble: 2 x 5
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ##          <dbl>       <dbl>        <dbl>       <dbl> <fct>  
    ## 1          5.1         3.5          1.4         0.2 setosa 
    ## 2          4.9         3            1.4         0.2 setosa

For instance, once a dataset is tidy, you are likely to reuse it several
times. You might also have a past analysis in GitHub, but you might not
want to clone, install dependencies and rerun your code just to access
your dataset. Another use case is to cross-join between datasets to
analyse across multiple projects or help you remember which datasets
you’ve used in the past.

While it’s useful to pin local datasets, pins really shine when working
with remote datasets.

### Remote Datasets

When analysing data, it is common to find code that makes use of
**remote files**. For
example,

``` r
sales <- read.csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv")
```

While this works fine most of the time, if the remote dataset is removed
or if you are working offline, the previous code will simply break.
Instead, you can `pin()` the remote dataset file before reading
it,

``` r
sales <- read.csv(pin("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv", "sales"))
```

While the code is similar, `pin()` will cache remote datasets and
trigger a warning when the `pin()` fails to refresh; this is much better
than having your code completely break in the absence of the remote
dataset, or worse, loosing the entire dataset if the remote location
breaks.

Some datasets are stored in **remote databases** which you usually
access with `DBI` or `dplyr`, while they don’t present the risk of being
deleted by mistake, there are also bennefits to using `pins` with
databases, see [Database Pins]().

## Discover

The `pins` package can help you discover interesting datasets by
specifying alternate boards. A board is the default storage location for
your pins, by default pins are saved and searched in your local machine,
but you can also search [CRAN](https://cran.r-project.org) packages and
[Kaggle](https://kaggle.com).

You can search CRAN datasets that contain “seattle” in their description
or name as follows:

``` r
find_pin("seattle", board = "packages")
```

    ## # A tibble: 4 x 4
    ##   name               description                               type  board 
    ##   <fct>              <fct>                                     <fct> <chr> 
    ## 1 hpiR_seattle_sales Seattle Home Sales from hpiR package.     table packa…
    ## 2 microsynth_seattl… Data for a crime intervention in Seattle… table packa…
    ## 3 vegawidget_data_s… Example dataset: Seattle daily weather f… table packa…
    ## 4 vegawidget_data_s… Example dataset: Seattle hourly temperat… table packa…

You can the retrieve a specific dataset with `get_pin()`:

``` r
get_pin("hpiR_seattle_sales", board = "packages")
```

    ## # A tibble: 43,313 x 16
    ##    pinx  sale_id sale_price sale_date  use_type  area lot_sf  wfnt
    ##    <chr> <chr>        <int> <date>     <chr>    <int>  <int> <dbl>
    ##  1 ..00… 2013..…     289000 2013-02-06 sfr         79   9295     0
    ##  2 ..00… 2013..…     356000 2013-07-11 sfr         18   6000     0
    ##  3 ..00… 2010..…     333500 2010-12-29 sfr         79   7200     0
    ##  4 ..00… 2016..…     577200 2016-03-17 sfr         79   7200     0
    ##  5 ..00… 2012..…     237000 2012-05-02 sfr         79   5662     0
    ##  6 ..00… 2014..…     347500 2014-03-11 sfr         79   5830     0
    ##  7 ..00… 2012..…     429000 2012-09-20 sfr         18  12700     0
    ##  8 ..00… 2015..…     653295 2015-07-21 sfr         79   7000     0
    ##  9 ..00… 2014..…     427650 2014-02-19 townhou…    79   3072     0
    ## 10 ..00… 2015..…     488737 2015-03-19 townhou…    79   3072     0
    ## # … with 43,303 more rows, and 8 more variables: bldg_grade <int>,
    ## #   tot_sf <int>, beds <int>, baths <dbl>, age <int>, eff_age <int>,
    ## #   longitude <dbl>, latitude <dbl>

To search Kaggle datasets with `find_pin()`, you will have to download
your token file from
[kaggle.com/me/account](https://kaggle.com/me/account) and then register
the Kaggle board by running:

``` r
register_board("kaggle", token = "<path-to-kaggle.json>")
```

From now on, you can search the Kaggle board suing `find_pin("seattle",
board = "kaggle")`, or you can also search all boards by not specyfing a
particular board:

``` r
find_pin("seattle")
```

    ## # A tibble: 23 x 4
    ##    name                     description                        type  board 
    ##    <chr>                    <chr>                              <chr> <chr> 
    ##  1 hpiR_seattle_sales       Seattle Home Sales from hpiR pack… table packa…
    ##  2 microsynth_seattledmi    Data for a crime intervention in … table packa…
    ##  3 vegawidget_data_seattle… Example dataset: Seattle daily we… table packa…
    ##  4 vegawidget_data_seattle… Example dataset: Seattle hourly t… table packa…
    ##  5 airbnb/seattle           Seattle Airbnb Open Data           files kaggle
    ##  6 aaronschlegel/seattle-p… Seattle Pet Licenses               files kaggle
    ##  7 shanelev/seattle-airbnb… Seattle Airbnb Listings            files kaggle
    ##  8 sam/seattle-crime        Seattle Police Reports             files kaggle
    ##  9 seattle-public-library/… Seattle Library Checkout Records   files kaggle
    ## 10 city-of-seattle/seattle… Seattle Trade Permits              files kaggle
    ## # … with 13 more rows

## Share

`pins` supports shared storage locations using boards. A board is an
storage location to store pins for personal use or to share them with
your team, or publicly with the world. Use `use_board()` to choose a
board, currently `rstudio` and `database` boards are supported; however,
the `pins` package provides an extensible API you can use to store pins
anywhere. In addition, you can share dataset pins with Python users
through the `pins` pip package.

Sharing pins in RStudio and Python is presented next, you can read about
sharing in database boards under [Using Databases]().

### RStudio Connect

**RStudio Connect** can be used to share pins within your organization,
to use this feature you will need to configure a
[publishing](https://docs.rstudio.com/connect/user/publishing.html)
account in RStudio.

``` r
use_board("rstudio")
```

When using multiple publishing servers, you can specify an specific
server through `use_board("rstudio", "<server-name>")`.

When using `pins` within RMarkdown documents that you want to run at a
given schedule, you’ll have to first retrieve your publishing secret
credentials:

``` r
get_board("rstudio")$secret()
```

Followed by specifying that secret in `use_board()`, this can be
securely accomplish by defining an environment variable `secret` with
the contents from the previous step in RStudio Connect, please treat
your credentials with care\!

``` r
use_board("rstudio", secret = Sys.getenv("secret"))
```

### Python

You can install `pins` using
`pip`:

``` bash
pip install git+https://github.com/rstudio/pins/#egg=pins\&subdirectory=python --user
```

You can then track your datasets privately with `pin()`,

``` python
import pins
import pandas as pd

pins.pin(pd.DataFrame({"a": [1, 2, 3]}), "python-df")
```

    ##      a
    ## 0  1.0
    ## 1  2.0
    ## 2  3.0

and retrieve them back with `get_pin()`.

``` python
pins.get_pin("iris-head")
```

    ##    Sepal.Length  Sepal.Width  Petal.Length  Petal.Width Species
    ## 0           5.1          3.5           1.4          0.2  setosa
    ## 1           4.9          3.0           1.4          0.2  setosa

You can search datasets that contain “seattle” in their description or
name as
    follows:

``` python
pins.find_pin("seattle_sales")
```

    ##                  name                            description   type     board
    ## 0  hpiR_seattle_sales  Seattle Home Sales from hpiR package.  table  packages

You can then retrieve a specific dataset with
    `get_pin()`:

``` python
pins.get_pin("hpiR_seattle_sales").head(5)
```

    ##            pinx      sale_id  sale_price  ... eff_age   longitude   latitude
    ## 0  ..0001800010   2013..2432      289000  ...       6 -122.312491  47.561380
    ## 1  ..0001800066  2013..21560      356000  ...      87 -122.322007  47.550353
    ## 2  ..0001800075  2010..24221      333500  ...      80 -122.311654  47.561470
    ## 3  ..0001800075   2016..6629      577200  ...      86 -122.311654  47.561470
    ## 4  ..0001800080   2012..9521      237000  ...      72 -122.309695  47.561472
    ## 
    ## [5 rows x 16 columns]

## RStudio

This package provides an [RStudio
Addin](https://rstudio.github.io/rstudio-extensions/rstudio_addins.html)
to search for datasets and an [RStudio
Connection](https://rstudio.github.io/rstudio-extensions/rstudio-connections.html)
extension to track local or remote datasets.

![](tools/readme/rstudio-pins-addmin.png)

The addin provides a list of datasets and visual clues that describe how
large and wide eachd dataset is.
