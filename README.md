pins: Track, Discover and Share Datasets
================

  - **Track** local and remote datasets using `pin()` and `get_pin()`.
  - **Discover** new datasets from packages, online and your
    organization using `find_pin()`.
  - **Share** datasets with your team, or the world, using customizable
    boards through `use_board()`.
  - **Extend** storage locations with custom boards, you decide where
    your data lives.

## R

You can install `pins` using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("rstudio/pins", subdir = "R")
```

You can then track your datasets privately with `pin()`,

``` r
library(dplyr, warn.conflicts = FALSE)
library(pins)

iris %>%
  filter(Sepal.Width < 3, Petal.Width < 1) %>%
  pin("iris-small-width", "A subset of 'iris' with only small widths.")
```

and retrieve them back with `get_pin()`.

``` r
get_pin("iris-small-width")
```

    ## # A tibble: 2 x 5
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ##          <dbl>       <dbl>        <dbl>       <dbl> <fct>
    ## 1          4.4         2.9          1.4         0.2 setosa
    ## 2          4.5         2.3          1.3         0.3 setosa

You can search datasets that contain “seattle” in their description or
name as follows:

``` r
find_pin("seattle")
```

    ## # A tibble: 4 x 4
    ##   name               description                               type  board
    ##   <chr>              <chr>                                     <chr> <chr>
    ## 1 hpiR_seattle_sales Seattle Home Sales from hpiR package.     table packa…
    ## 2 microsynth_seattl… Data for a crime intervention in Seattle… table packa…
    ## 3 vegawidget_data_s… Example dataset: Seattle daily weather f… table packa…
    ## 4 vegawidget_data_s… Example dataset: Seattle hourly temperat… table packa…

You can then retrieve a specific dataset with `get_pin()`:

``` r
get_pin("hpiR_seattle_sales")
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

Please reference the [R README](R/README.md) for additional functionality and details.

## Python

You can install `pins` using `pip`:

``` python
pip install git+https://github.com/rstudio/pins/#egg=pins\&subdirectory=python --user
```

You can then track your datasets privately with `pin()`,

``` r
import pins

# TODO: pins.pin()
```

and retrieve them back with `get_pin()`.

``` r
pins.get_pin("iris-small-width")
```

    ## TODO

You can search datasets that contain “seattle” in their description or
name as follows:

``` r
pins.find_pin("seattle")
```

    ## # A tibble: 4 x 4
    ##   name               description                               type  board
    ##   <chr>              <chr>                                     <chr> <chr>
    ## 1 hpiR_seattle_sales Seattle Home Sales from hpiR package.     table packa…
    ## 2 microsynth_seattl… Data for a crime intervention in Seattle… table packa…
    ## 3 vegawidget_data_s… Example dataset: Seattle daily weather f… table packa…
    ## 4 vegawidget_data_s… Example dataset: Seattle hourly temperat… table packa…

You can then retrieve a specific dataset with `get_pin()`:

``` r
pins.get_pin("hpiR_seattle_sales")
```

    ## TODO

Please reference the [Python README](Python/README.md) for additional functionality and details.

