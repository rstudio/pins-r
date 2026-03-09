# Managing custom formats

The pins package provides a robust set of functions to read and write
standard types of files using standard tools, e.g. CSV files using
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) and
[`write.csv()`](https://rdrr.io/r/utils/write.table.html). However, from
time to time, you may wish read or write in other ways. You may want to
read and write:

- CSV files using readr or vroom
- Arrow files without using compression
- Whole directories that are archived/zipped

You can create a customized approach using either
[`pin_upload()`](https://pins.rstudio.com/reference/pin_download.md) and
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md).
The goal of this vignette is to show how you can incorporate this
customization into your workflow. To see a different approach for when
you want to write and read with consistent metadata, see
[`vignette("customize-pins-metadata")`](https://pins.rstudio.com/articles/customize-pins-metadata.md).

We’ll begin with an example where we write and read uncompressed Arrow
files, starting by creating a temporary board:

``` r
library(pins)

board <- board_temp()
```

## Upload a single file

Two points to keep in mind:

- [`pin_upload()`](https://pins.rstudio.com/reference/pin_download.md)
  takes a vector of `paths` to local files.
- [`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
  returns a vector of `paths` to local files.

If you are writing a one-off file, you can do everything directly:

``` r
pin_name <- "mtcars-arrow"

# file name will be `mtcars-arrow.arrow`
path <- fs::path_temp(fs::path_ext_set(pin_name, "arrow"))

arrow::write_feather(mtcars, path, compression = "uncompressed")

pin_upload(board, paths = path, name = pin_name)
#> Creating new version '20260309T143720Z-b1934'
```

Reading from the downloaded pin is straightforward;
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
returns a local path that can be piped to
[`arrow::read_feather()`](https://arrow.apache.org/docs/r/reference/read_feather.html):

``` r
mtcars_download <- 
  pin_download(board, pin_name) |>
  arrow::read_feather()

head(mtcars_download)
#> # A tibble: 6 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
```

## Function to manage uploading

If you want to write more than one custom file of a certain type, or
using a certain tool, you might consider writing a helper function:

``` r
pin_upload_arrow <- function(board, x, name, ...) {
  # path deleted when `pin_upload_arrow()` exits
  path <- fs::path_temp(fs::path_ext_set(name, "arrow"))
  withr::defer(fs::file_delete(path))
  
  # custom writer
  arrow::write_feather(x, path, compression = "uncompressed")
  
  pin_upload(board, paths = path, name = name, ...) 
}
```

This helper function is designed to work like
[`pin_write()`](https://pins.rstudio.com/reference/pin_read.md):

``` r
pin_upload_arrow(board, x = mtcars, name = "mtcars-arrow2")
#> Creating new version '20260309T143720Z-b1934'
```

As before, you can pipe the result of
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
to your reader function:

``` r
pin_download(board, name = "mtcars-arrow2") |>
  arrow::read_feather() |>
  head()
#> # A tibble: 6 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
#> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
#> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
#> 5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2
#> 6  18.1     6   225   105  2.76  3.46  20.2     1     0     3     1
```

## Another example: upload a zipped directory archive as a pin

If you want to use this same approach to
[archive](https://archive.r-lib.org/) and pin a whole directory, you can
write a helper function like:

``` r
pin_upload_archive <- function(board, dir, name, ...) {
  path <- fs::path_temp(fs::path_ext_set(name, "tar.gz"))
  withr::defer(fs::file_delete(path))
  archive::archive_write_dir(path, dir)
  pin_upload(board = board, paths = path, name = name, ...)
}
```

You can download the compressed archive via `pin_download(board, name)`
and then pipe that path straight to
[`archive::archive_extract()`](https://archive.r-lib.org/reference/archive_extract.html)
to extract your archive in a new directory.
