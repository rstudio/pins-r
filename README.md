pins: Track, Discover and Share Datasets
================

[![Build
Status](https://travis-ci.org/javierluraschi/pins.svg?branch=master)](https://travis-ci.org/javierluraschi/pins)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pins)](https://cran.r-project.org/package=pins)

  - **Track** local and remote datasets using `pin()` and `get_pin()`.
  - **Discover** new datasets from R packages, online and your
    organization using `find_pin()`.
  - **Share** datasets with your team, or the world, using customizable
    boards through `use_board()`.
  - **Extend** storage locations with custom **boards**, you decide
    where your data lives.

## Installation

You can install `pins` using the `remotes` package:

``` r
install.packages("remotes")
remotes::install_github("javierluraschi/pins")
```

## Tracking Datasets

It’s easy to track local datasets using `pin()` and `get_pin()`. In
addition, you can retrive remote datasets using `DBI` or pin remote
datasets without retrieving them using `dplyr`.

### Local Datasets

You can track your datasets privately by pinning with `pin()`.

``` r
library(dplyr, warn.conflicts = FALSE)
library(pins)

iris %>%
  filter(Sepal.Width < 3, Petal.Width < 1) %>%
  pin("iris-small-width", "A subset of 'iris' with only small widths.")
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          4.4         2.9          1.4         0.2  setosa
    ## 2          4.5         2.3          1.3         0.3  setosa

You can then retrieve them back with `get_pin()`.

``` r
get_pin("iris-small-width")
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          4.4         2.9          1.4         0.2  setosa
    ## 2          4.5         2.3          1.3         0.3  setosa

A pin is a tool to help you organize your datasets to easily fetch
results from past data analysis sessions.

For instance, once a dataset is tidy, you are likely to reuse it several
times. You might also have a past analysis in GitHub, but you might not
want to clone, install dependencies and rerun your code just to access
your dataset. Another use case is to cross-join between datasets to
analyse across multiple projects or help you remember which datasets
you’ve used in the past.

### Remote Datasets

Some datasets are stored in datasets which you usually access with `DBI`
or `dplyr`. For instance, you might want to access a public dataset
stored in
`bigrquery`:

``` r
con <- DBI::dbConnect(bigrquery::bigquery(), project = bq_project, dataset = bq_dataset)
```

Which we can analyze with `DBI` and then pin the results locally:

``` r
DBI::dbGetQuery(con, "
  SELECT score, count(*) as n
  FROM (SELECT 10 * floor(score/10) as score FROM `bigquery-public-data.hacker_news.full`)
  GROUP BY score") %>%
  pin("hacker-news-scores", "Hacker News scores grouped by tens.")
```

However, you can only use `DBI` when you can fetch all the data back
into R, this is not feasible in many cases. Instead, when using `dplyr`,
you can pin large datasets and transform them without having to fetch
any data at all.

Lets pin the entire dataset using `dplyr`:

``` r
tbl(con, "bigquery-public-data.hacker_news.full") %>%
  pin("hacker-news-full")
```

    ## # Source:   SQL [?? x 14]
    ## # Database: BigQueryConnection
    ##    by    score   time timestamp           title type  url   text  parent
    ##    <chr> <int>  <int> <dttm>              <chr> <chr> <chr> <chr>  <int>
    ##  1 MrMe…    NA 1.36e9 2013-03-03 23:04:40 ""    comm… ""    "The… 5.32e6
    ##  2 elor…    NA 1.44e9 2015-07-09 16:39:07 ""    comm… ""    We d… 9.86e6
    ##  3 rlpb     NA 1.27e9 2010-04-29 06:02:03 ""    comm… ""    It's… 1.30e6
    ##  4 jboy…    NA 1.46e9 2016-04-19 07:54:35 ""    comm… ""    "Sli… 1.15e7
    ##  5 nivs…    NA 1.37e9 2013-05-22 12:10:05 ""    comm… ""    Noth… 5.75e6
    ##  6 RexR…    NA 1.44e9 2015-09-17 00:15:00 ""    comm… ""    It w… 1.02e7
    ##  7 Estr…    NA 1.24e9 2009-06-07 13:50:13 ""    comm… ""    Ther… 6.46e5
    ##  8 stre…    NA 1.30e9 2011-03-25 13:43:20 ""    comm… ""    TLDR… 2.37e6
    ##  9 dman     NA 1.38e9 2013-08-08 22:40:08 ""    comm… ""    Whis… 6.18e6
    ## 10 tedu…    NA 1.54e9 2018-09-18 19:05:30 ""    comm… ""    Seem… 1.80e7
    ## # … with more rows, and 5 more variables: deleted <lgl>, dead <lgl>,
    ## #   descendants <int>, id <int>, ranking <int>

This works well if you provide the connection, after your R session gets
restarted, you would have to provide a connection yourself before
retrieving the
pin:

``` r
con <- DBI::dbConnect(bigrquery::bigquery(), project = bq_project, dataset = bq_dataset)
get_pin("hacker-news-full")
```

This is acceptable but not ideal – it’s hard to remember what connection
to use for each dataset. So instead, pin a
connection:

``` r
con <- pin(~DBI::dbConnect(bigrquery::bigquery(), project = bq_project, dataset = bq_dataset), "bigquery")
```

Then pin your dataset as you would usually
would,

``` r
tbl(con, "bigquery-public-data.hacker_news.full") %>% pin("hacker-news-full")
```

From now on, after restarting your R session and retrieving the pin, the
pin will initialize the connection before retrieving a `dplyr` reference
to it with `pin("hacker-news-full")`.

Which in turn, allows you to further process the datset using `dplyr`
and pin additional remote datasets.

``` r
get_pin("hacker-news-full") %>%
  transmute(score = 10 * floor(score/10)) %>%
  group_by(score) %>%
  summarize(n = n()) %>%
  filter(score < 2000) %>%
  pin("hacker-news-scores")
```

You can then use this `dplyr` pin to process data further; for instance,
by visualizing it with ease:

``` r
library(ggplot2)

get_pin("hacker-news-scores") %>%
  ggplot() +
    geom_bar(aes(x = score, y = n), stat="identity") +
    scale_y_log10() + theme_light()
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

You can also cache this dataset locally by running `collect()` on the
pin and then re-pinning it with `pin()`.

## Sharing Datasets

`pins` supports shared storage locations using boards. A board is a
remote location for you to share pins with your team privately, or with
the world, publicly. Use `use_board()` to choose a board, currently only
databases are supported; however, `pins` provide an extensible API you
can use to store pins anywhere.

### Databases

We can reuse our `bigrquery` connection to define a database-backed
shared board,

``` r
use_board("database", con)
```

Which we can also use to pin a dataset,

``` r
pin(iris, "iris", "The entire 'iris' dataset.")
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

find pins,

``` r
find_pin()
```

    ##   name                description
    ## 1 iris The entire 'iris' dataset.

and retrieve shared datasets.

``` r
get_pin("iris")
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
