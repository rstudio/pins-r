# pins

The pins package publishes data, models, and other R objects, making it
easy to share them across projects and with your colleagues. You can pin
objects to a variety of pin *boards*, including folders (to share on a
networked drive or with services like DropBox), Posit Connect,
Databricks, Amazon S3, Google Cloud Storage, Azure storage, and
Microsoft 365 (OneDrive and SharePoint). Pins can be automatically
versioned, making it straightforward to track changes, re-run analyses
on historical data, and undo mistakes.

You can use pins from Python as well as R. For example, you can use one
language to read a pin created with the other. Learn more about [pins
for Python](https://rstudio.github.io/pins-python/).

## Installation

You can install pins from CRAN with:

``` r

install.packages("pins")
```

You can install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("rstudio/pins-r")
```

## Usage

To use the pins package, you must first create a pin board. A good place
to start is
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md),
which stores pins in a directory you specify. Here I’ll use a special
version of
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
called
[`board_temp()`](https://pins.rstudio.com/dev/reference/board_folder.md)
which creates a temporary board that’s automatically deleted when your R
session ends. This is great for examples, but obviously you shouldn’t
use it for real work!

``` r

library(pins)

board <- board_temp()
board
#> Pin board <pins_board_folder>
#> Path:
#> '/var/folders/hl/v1lzqxfd07b3hgd2tt5cjcs40000gp/T/Rtmpp4kE0d/pins-2a644bd98c4c'
#> Cache size: 0
```

You can “pin” (save) data to a board with
[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md). It
takes three arguments: the board to pin to, an object, and a name:

``` r

board |> pin_write(head(mtcars), "mtcars")
#> Guessing `type = 'parquet'`
#> Creating new version '20260313T164232Z-c8df2'
#> Writing to pin 'mtcars'
```

As you can see, the data frame was saved as a `.parquet` file by
default. Depending on what you’re saving and who else you want to read
it, you might use the `type` argument to instead save it as an RDS,
Arrow, CSV, or JSON file.

You can later retrieve the pinned data with
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md):

``` r

board |> pin_read("mtcars")
#> # A data frame: 6 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
```

A board on your computer is good place to start, but the real power of
pins comes when you use a board that’s shared with multiple people. To
get started, you can use
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
with a directory on a shared drive or in dropbox, or if you use [Posit
Connect](https://posit.co/products/enterprise/connect/) you can use
[`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md):

``` r

board <- board_connect()
#> Connecting to Posit Connect 2024.08.0 at <https://pub.current.posit.team>
board |> pin_write(tidy_sales_data, "sales-summary", type = "rds")
#> Writing to pin 'hadley/sales-summary'
```

Then, someone else (or an automated Quarto report) can read and use your
pin:

``` r

board <- board_connect()
board |> pin_read("hadley/sales-summary")
```

You can easily control who gets to access the data using the Posit
Connect permissions pane.

The pins package also includes boards that allow you to share data on
services like Databricks Volumes
([`board_databricks()`](https://pins.rstudio.com/dev/reference/board_databricks.md)),
Amazon’s S3
([`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md)),
Azure’s blob storage
([`board_azure()`](https://pins.rstudio.com/dev/reference/board_azure.md)),
and Google Cloud Storage
([`board_gcs()`](https://pins.rstudio.com/dev/reference/board_gcs.md)).
Learn more in
[`vignette("pins")`](https://pins.rstudio.com/dev/articles/pins.md).
