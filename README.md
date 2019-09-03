pins: Pin, Discover and Share Resources
================

# pins: Pin, Discover and Share Resources

[![Build
Status](https://travis-ci.org/rstudio/pins.svg?branch=master)](https://travis-ci.org/rstudio/pins)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rstudio/pins?branch=master&svg=true)](https://ci.appveyor.com/project/rstudio/pins)
[![CRAN
Status](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)
[![Code
Coverage](https://codecov.io/gh/rstudio/pins/branch/master/graph/badge.svg)](https://codecov.io/gh/rstudio/pins)
[![Downloads](https://cranlogs.r-pkg.org/badges/pins?color=blue)](https://cranlogs.r-pkg.org/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Chat](https://badges.gitter.im/rstudio/pins.svg)](https://gitter.im/rstudio/sparklyr?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![GitHub
Stars](https://img.shields.io/github/stars/rstudio/pins.svg)](https://github.com/rstudio/pins/stargazers)

## Overview

You can use the `pins` package to:

  - **Pin** remote resources locally with `pin()`, work offline and
    cache results.
  - **Discover** new resources across different boards using
    `pin_find()`.
  - **Share** resources in local folders, GitHub, Kaggle, and RStudio
    Connect by registering new boards with `board_register()`.

## Installation

``` r
# Install the released version from CRAN:
install.packages("pins")
```

## Usage

``` r
library(pins)
```

### Pin

There are two main ways pin a resource:

  - Pin a remote file with `pin(url)`. This will download the file and
    make it available in a local cache:
    
    ``` r
    url <- "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv"
    retail_sales <- read.csv(pin(url))
    ```
    
    This makes subsequent uses much faster and allows you to work
    offline. If the resource changes, `pin()` will automatically
    re-download it; if goes away, `pin()` will keep the local cache.

  - Pin an expensive local computation with `pin(object, name)`:
    
    ``` r
    library(dplyr)
    retail_sales %>%
      group_by(month = lubridate::month(ds, T)) %>%
      summarise(total = sum(y)) %>%
      pin("sales_by_month")
    ```
    
    Then later retrieve it with `pin_get(name)`.
    
    ``` r
    head(pin_get("sales_by_month"))
    #> # A tibble: 6 x 2
    #>   month   total
    #>   <ord>   <int>
    #> 1 Jan   6896303
    #> 2 Feb   6890866
    #> 3 Mar   7800074
    #> 4 Apr   7680417
    #> 5 May   8109219
    #> 6 Jun   7451431
    ```

### Discover

You can also discover remote resources using `pin_find()`. It can search
for resources in CRAN packages, Kaggle, and RStudio Connect. For
instance, we can search datasets mentioning “seattle” in CRAN packages
with:

``` r
pin_find("seattle", board = "packages")
#> # A tibble: 6 x 4
#>   name               description                               type  board 
#>   <chr>              <chr>                                     <chr> <chr> 
#> 1 hpiR/ex_sales      Subset of Seattle Home Sales from hpiR p… table packa…
#> 2 hpiR/seattle_sales Seattle Home Sales from hpiR package.     table packa…
#> 3 latticeExtra/Seat… Daily Rainfall and Temperature at the Se… table packa…
#> 4 microsynth/seattl… Data for a crime intervention in Seattle… table packa…
#> 5 vegawidget/data_s… Example dataset: Seattle daily weather f… table packa…
#> 6 vegawidget/data_s… Example dataset: Seattle hourly temperat… table packa…
```

Notice that the full name of a pin is `<owner>/<name>`. This namespacing
allows multiple people (or packages) to create pins with the same name.

You can then retrieve a pin through `pin_get()`:

``` r
sales <- pin_get("hpiR/seattle_sales")
head(sales)
#> # A tibble: 6 x 16
#>   pinx  sale_id sale_price sale_date  use_type  area lot_sf  wfnt
#>   <chr> <chr>        <int> <date>     <chr>    <int>  <int> <dbl>
#> 1 ..00… 2013..…     289000 2013-02-06 sfr         79   9295     0
#> 2 ..00… 2013..…     356000 2013-07-11 sfr         18   6000     0
#> 3 ..00… 2010..…     333500 2010-12-29 sfr         79   7200     0
#> 4 ..00… 2016..…     577200 2016-03-17 sfr         79   7200     0
#> 5 ..00… 2012..…     237000 2012-05-02 sfr         79   5662     0
#> 6 ..00… 2014..…     347500 2014-03-11 sfr         79   5830     0
#> # … with 8 more variables: bldg_grade <int>, tot_sf <int>, beds <int>,
#> #   baths <dbl>, age <int>, eff_age <int>, longitude <dbl>, latitude <dbl>
```

### Share

Finally, you can share resources with other R sessions and other users
by publishing to a local folder, Kaggle, GitHub and RStudio Connect. To
share with other R sessions, you can use a local board which stores pins
in a shared path, usually `~/.pins`:

``` r
board_register_local(cache = "~/pins")
```

To publish to Kaggle, you would first need to register the Kaggle board
by creating a [Kaggle API Token](https://www.kaggle.com/me/account):

``` r
board_register_kaggle(token = "<path-to-kaggle.json>")
```

You can then easily publish to Kaggle:

``` r
pin(sales, name = "seattle_sales", board = "kaggle")
```

Learn more in `vignette("boards-understanding")`

### RStudio

Experimental support for `pins` was introduced in RStudio Connect 1.7.8
so that you can use [RStudio](https://www.rstudio.com/products/rstudio/)
and [RStudio Connect](https://www.rstudio.com/products/connect/) to
discover and share resources within your organization with ease. To
enable new boards, use [RStudio’s Data
Connections](https://blog.rstudio.com/2017/08/16/rstudio-preview-connections/)
to start a new ‘pins’ connection and then select which board to connect
to:

<center>

![](tools/readme/rstudio-connect-board.png)

</center>

Once connected, you can use the connections pane to track the pins you
own and preview them with ease. Notice that one connection is created
for each board.

<center>

![](tools/readme/rstudio-explore-pins.png)

</center>

To **discover** remote resources, simply expand the “Addins” menu and
select “Find Pin” from the dropdown. This addin allows you to search for
pins across all boards, or scope your search to particular ones as well:

<center>

![](tools/readme/rstudio-discover-pins.png)

</center>

You can then **share** local resources using the RStudio Connect board.
Lets use `dplyr` and the `hpiR_seattle_sales` pin to analyze this
further and then pin our results in RStudio Connect.

``` r
board_register_rsconnect(name = "myrsc")
```

``` r
sales %>%
  group_by(baths = ceiling(baths)) %>%
  summarise(sale = floor(mean(sale_price))) %>%
  pin("sales-by-baths", board = "myrsc")
#> # A tibble: 8 x 2
#>   baths    sale
#>   <dbl>   <dbl>
#> 1     1  413950
#> 2     2  516480
#> 3     3  638674
#> 4     4  939602
#> 5     5 1748859
#> 6     6 3384514
#> 7     7 3063043
#> 8     8 4550750
```

After a pin is published, you can then browse to the pin’s content from
the RStudio Connect web interface.

<center>

![](tools/readme/rstudio-share-resources.png)

</center>

You can now set the appropriate permissions in RStudio Connect, and
voila\! From now on, those with access can make use of this remote file
locally\!

For instance, a colleague can reuse the `sales-by-baths` pin by
retrieving it from RStudio Connect and visualize its contents using
ggplot2:

``` r
library(ggplot2)
board_register_rsconnect(name = "myrsc")

pin_get("sales-by-baths", board = "myrsc") %>%
  ggplot(aes(x = baths, y = sale)) +
  geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ exp(x))
```

<img src="tools/readme/rstudio-plot-pin-1.png" style="display: block; margin: auto;" />

Pins can also be automated using scheduled R Markdown. This makes it
much easier to create Shiny applications that rely on scheduled data
updates or to share prepared resources across multiple pieces of
content. You no longer have to fuss with file paths on RStudio Connect,
mysterious resource URLs, or redeploying application code just to update
a dataset\!

### Python

Experimental support for pins is also available in Python. However,
since the Python interface currently makes use of the R package, the R
runtime needs to be installed when using pins from Python. To get
started, first install the pins module:

``` bash
pip install git+https://github.com/rstudio/pins.git@v0.1.0#subdirectory=python
```

Followed by using `pins` from Python:

``` python
import pins
pins.pin_get("hpiR/seattle_sales")
```

Please make sure to ~~pin~~ visit,
[rstudio.github.io/pins](https://rstudio.github.io/pins/index.html),
where you will find detailed documentation and additional resources.
