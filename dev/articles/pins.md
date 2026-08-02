# Get started with pins

The pins package helps you publish data sets, models, and other R
objects, making it easy to share them across projects and with your
colleagues. You can pin objects to a variety of “boards”, including
local folders (to share on a networked drive or with dropbox), Posit
Connect, Amazon S3, and more. This vignette will introduce you to the
basics of pins.

``` r

library(pins)
```

## Getting started

Every pin lives in a pin *board*, so you must start by creating a pin
board. In this vignette I’ll use a temporary board which is
automatically deleted when your R session is over:

``` r

board <- board_temp()
```

In real-life, you’d pick a board depending on how you want to share the
data. Here are a few options:

``` r
board <- board_local() # share data across R sessions on the same computer
board <- board_folder("~/Dropbox") # share data with others using dropbox
board <- board_folder("Z:\\my-team\pins") # share data using a shared network drive
board <- board_connect() # share data with Posit Connect
```

## Reading and writing data

Once you have a pin board, you can write data to it with
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md):

``` r

mtcars <- tibble::as_tibble(mtcars)
board |> pin_write(mtcars, "mtcars")
#> Guessing `type = 'parquet'`
#> Creating new version '20260802T194617Z-c0340'
#> Writing to pin 'mtcars'
```

The first argument is the object to save (usually a data frame, but it
can be any R object), and the second argument gives the “name” of the
pin. The name is basically equivalent to a file name: you’ll use it when
you later want to read the data from the pin. The only rule for a pin
name is that it can’t contain slashes.

After you’ve pinned an object, you can read it back with
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md):

``` r

board |> pin_read("mtcars")
#> # A data frame: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
```

You don’t need to supply the file type when reading data from a pin
because pins automatically stores the file type in the
[metadata](#metadata).

## How and what to store as a pin

As you can see from the output in the previous section, pins has chosen
to save this example data to an `.rds` file. But you can choose another
option depending on your goals:

- `type = "rds"` uses `writeRDS()` to create a binary R data file. It
  can save any R object (including trained models) but it’s only
  readable from R, not other languages.
- `type = "csv"` uses
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html) to create a
  CSV file. CSVs are plain text and can be read easily by many
  applications, but they only support simple columns (e.g. numbers,
  strings), can take up a lot of disk space, and can be slow to read.
- `type = "parquet"` uses
  [`nanoparquet::write_parquet()`](https://nanoparquet.r-lib.org/reference/write_parquet.html)
  to create a Parquet file. [Parquet](https://parquet.apache.org/) is a
  modern, language-independent, column-oriented file format for
  efficient data storage and retrieval. Parquet is an excellent choice
  for storing tabular data but requires the
  [nanoparquet](https://nanoparquet.r-lib.org/) package.
- `type = "arrow"` uses
  [`arrow::write_feather()`](https://arrow.apache.org/docs/r/reference/write_feather.html)
  to create an Arrow/Feather file.
- `type = "json"` uses
  [`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
  to create a JSON file. Pretty much every programming language can read
  json files, but they only work well for nested lists.
- `type = "qs2"` uses
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html) to create
  a binary R data file. The [qs2](https://github.com/qsbase/qs2) format
  achieves faster read/write speeds than RDS, and compresses data more
  efficiently, making it a good choice for larger objects.

Note that when the data lives elsewhere, pins takes care of downloading
and caching so that it’s only re-downloaded when needed. That said, most
boards transmit pins over HTTP, and this is going to be slow and
possibly unreliable for very large pins. As a general rule of thumb, we
don’t recommend using pins with files over 500 MB. If you find yourself
routinely pinning data larger that this, you might need to reconsider
your data engineering pipeline.

Storing your data/object as a pin works well when you write from a
single source or process. It is *not* appropriate when multiple sources
or processes need to write to the same pin; since the pins package reads
and writes files, it cannot manage concurrent writes.

- **Good** use for pins: an ETL pipeline that stores a model or
  summarized dataset once a day
- **Bad** use for pins: a Shiny app that collects data from users, who
  may be using the app at the same time

## Metadata

Every pin is accompanied by some metadata that you can access with
[`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md):

``` r

board |> pin_meta("mtcars")
#> List of 13
#>  $ file       : chr "mtcars.parquet"
#>  $ file_size  : 'fs_bytes' int 2.91K
#>  $ pin_hash   : chr "c03402d9ca7322e7"
#>  $ type       : chr "parquet"
#>  $ title      : chr "mtcars: a pinned 32 x 11 data frame"
#>  $ description: NULL
#>  $ tags       : NULL
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-08-02 19:46:17"
#>  $ api_version: int 1
#>  $ user       : list()
#>  $ name       : chr "mtcars"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/Rtmpz2JpwS/pins-237d32368b5b/mtcars/20260802T194617Z-c0340"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260802T194617Z-c0340"
```

This shows you the metadata that’s generated by default. This includes:

- `title`, a brief textual description of the dataset.

- an optional `description`, where you can provide more details.

- the date-time when the pin was `created`.

- the `file_size`, in bytes, of the underlying files.

- a unique `pin_hash` that you can supply to
  [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) to
  ensure that you’re reading exactly the data that you expect.

When creating the pin, you can override the default description or
provide additional metadata that is stored with the data:

``` r

board |> pin_write(mtcars, 
  description = "Data extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).",
  metadata = list(
    source = "Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411."
  ),
  # Necessary if only changing pin metadata but pin content is the same
  force_identical_write = TRUE
)
#> Using `name = 'mtcars'`
#> Guessing `type = 'parquet'`
#> Replacing version '20260802T194617Z-c0340' with
#> '20260802T194618Z-c0340'
#> Writing to pin 'mtcars'
board |> pin_meta("mtcars")
#> List of 13
#>  $ file       : chr "mtcars.parquet"
#>  $ file_size  : 'fs_bytes' int 2.91K
#>  $ pin_hash   : chr "c03402d9ca7322e7"
#>  $ type       : chr "parquet"
#>  $ title      : chr "mtcars: a pinned 32 x 11 data frame"
#>  $ description: chr "Data extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobi"| __truncated__
#>  $ tags       : NULL
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-08-02 19:46:18"
#>  $ api_version: int 1
#>  $ user       :List of 1
#>   ..$ source: chr "Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411."
#>  $ name       : chr "mtcars"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/Rtmpz2JpwS/pins-237d32368b5b/mtcars/20260802T194618Z-c0340"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260802T194618Z-c0340"
```

While we’ll do our best to keep the automatically generated metadata
consistent over time, I’d recommend manually capturing anything you
really care about in `metadata`.

## Versioning

In many situations it’s useful to version pins, so that writing to an
existing pin does not replace the existing data, but instead adds a new
copy. There are two ways to turn versioning on:

- When you create a board you can turn versioning on for every pin in
  that board:

  ``` r

  board2 <- board_temp(versioned = TRUE)
  ```

- When you write a pin, you can specifically request that versioning be
  turned on for that pin:

  ``` r

  board2 <- board_temp()
  board2 |> pin_write(mtcars, versioned = TRUE)
  ```

Most boards have versioning on by default. The primary exception is
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
since that stores data on your computer, and there’s no automated way to
clean up the data you’re saving.

Once you have turned versioning on, every
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) will
create a new version:

``` r

board2 <- board_temp(versioned = TRUE)

board2 |> pin_write(1:5, name = "x", type = "rds")
#> Creating new version '20260802T194619Z-1d21e'
#> Writing to pin 'x'
board2 |> pin_write(2:6, name = "x", type = "rds")
#> Creating new version '20260802T194619Z-76e90'
#> Writing to pin 'x'
board2 |> pin_write(3:7, name = "x", type = "rds")
#> Creating new version '20260802T194619Z-9fb95'
#> Writing to pin 'x'
```

You can list all the available versions with
[`pin_versions()`](https://pins.rstudio.com/dev/reference/pin_versions.md):

``` r

board2 |> pin_versions("x")
#> # A tibble: 3 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260802T194619Z-1d21e 2026-08-02 19:46:19 1d21e
#> 2 20260802T194619Z-76e90 2026-08-02 19:46:19 76e90
#> 3 20260802T194619Z-9fb95 2026-08-02 19:46:19 9fb95
```

You can delete a specific older version with
[`pin_version_delete()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
or sets of older versions with
[`pin_versions_prune()`](https://pins.rstudio.com/dev/reference/pin_versions.md).

By default,
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) will
return the most recent version:

``` r

board2 |> pin_read("x")
#> [1] 3 4 5 6 7
```

But you can request an older version by supplying the `version`
argument:

``` r

board2 |> pin_read("x", version = "20210520T173110Z-49519")
```

## Reading and writing files

So far we’ve focussed on
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) which
work with R objects. pins also provides the lower-level
[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
and
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
which work with files on disk. You can use them to share types of data
that are otherwise unsupported by pins.

[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
works like
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) but
instead of an R object you give it a vector of paths. I’ll start by
creating a few files in the temp directory:

``` r

paths <- file.path(tempdir(), c("mtcars.csv", "alphabet.txt"))
write.csv(mtcars, paths[[1]])
writeLines(letters, paths[[2]])
```

Now I can upload those to the board:

``` r

board |> pin_upload(paths, "example")
#> Creating new version '20260802T194619Z-28b87'
```

[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
returns a vector of paths:

``` r

board |> pin_download("example")
#> [1] "/tmp/Rtmpz2JpwS/pins-237d32368b5b/example/20260802T194619Z-28b87/mtcars.csv"  
#> [2] "/tmp/Rtmpz2JpwS/pins-237d32368b5b/example/20260802T194619Z-28b87/alphabet.txt"
```

It’s now your job to handle them. You should treat these paths as
internal implementation details — never modify them and never save them
for use outside of pins.

Note that you can’t
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md)
something you pinned with
[`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md):

``` r

board |> pin_read("example")
#> Error in `object_read()`:
#> ! Cannot automatically read pin:
#> • Is your pin specified as a full path? Retrieve it with
#>   `pin_download()`
#> • Is your pin specified via a URL that is not a full path, such as a
#>   Posit Connect vanity URL? Remember to include a trailing slash `/`
```

But you can
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
something that you’ve pinned with
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md):

``` r

board |> pin_download("mtcars")
#> [1] "/tmp/Rtmpz2JpwS/pins-237d32368b5b/mtcars/20260802T194618Z-c0340/mtcars.parquet"
```

## Caching

The primary purpose of pins is to make it easy to share data. But pins
is also designed to help you spend as little time as possible
downloading data.
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
automatically cache remote pins: they maintain a local copy of the data
(so it’s fast) but always check that it’s up-to-date (so your analysis
doesn’t use stale data).

Wouldn’t it be nice if you could take advantage of this feature for any
dataset on the internet? That’s the idea behind
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) —
you can assemble your own board from datasets, wherever they live on the
internet. For example, this code creates a board containing a single
pin, `penguins`, that refers to some fun data I found on GitHub:

``` r

my_data <- board_url(c(
  "penguins" = "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins_raw.csv"
))
```

You can read this data by combining
[`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
with [`read.csv()`](https://rdrr.io/r/utils/read.table.html)[^1]:

``` r

my_data |>
  pin_download("penguins") |> 
  read.csv(check.names = FALSE) |> 
  tibble::as_tibble()
#> # A tibble: 344 × 17
#>    studyName `Sample Number` Species                Region Island Stage
#>    <chr>               <int> <chr>                  <chr>  <chr>  <chr>
#>  1 PAL0708                 1 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  2 PAL0708                 2 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  3 PAL0708                 3 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  4 PAL0708                 4 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  5 PAL0708                 5 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  6 PAL0708                 6 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  7 PAL0708                 7 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  8 PAL0708                 8 Adelie Penguin (Pygos… Anvers Torge… Adul…
#>  9 PAL0708                 9 Adelie Penguin (Pygos… Anvers Torge… Adul…
#> 10 PAL0708                10 Adelie Penguin (Pygos… Anvers Torge… Adul…
#> # ℹ 334 more rows
#> # ℹ 11 more variables: `Individual ID` <chr>,
#> #   `Clutch Completion` <chr>, `Date Egg` <chr>,
#> #   `Culmen Length (mm)` <dbl>, `Culmen Depth (mm)` <dbl>,
#> #   `Flipper Length (mm)` <int>, `Body Mass (g)` <int>, Sex <chr>,
#> #   `Delta 15 N (o/oo)` <dbl>, `Delta 13 C (o/oo)` <dbl>,
#> #   Comments <chr>
```

[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
requires a bit of work compared to using
[`download.file()`](https://rdrr.io/r/utils/download.file.html) or
similar but it has a big payoff: the data will only be re-downloaded
when it changes.

[^1]: Here I’m using
    [`read.csv()`](https://rdrr.io/r/utils/read.table.html) to the
    reduce the dependencies of the pins package. For real code I’d
    recommend using
    [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
    or `readr::read_csv().`
