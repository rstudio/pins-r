# Using web-hosted boards

The pins package supports back-and-forth collaboration for publishing
and consuming using, for example,
[`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md). The
goal of this vignette is to show how to publish a board of pins to a
website, bringing your pins to a wider audience. How does this work?

- For consumers,
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  offers **read-only** collaboration.
- For publishers, a helper function is provided,
  [`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md).
  A manifest contains a list of pins and versions, enabling a
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  to read like a
  [`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md) or
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md).

## Publishing

The steps for publishing a board that can be read by consumers using
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
are:

- create a board,
- write pins to the board,
- write a manifest file using
  [`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md),
  and
- publish your website.

The first and last steps will be specific to how you deploy your board
on the web; we discuss options in the [Publishing
platforms](#publishing-platforms) section. Regardless of platform,
you’ll write the pins and the manifest the same way.

For this first demonstration, we’ll start by creating a board, and
finish by showing how the board works after being served.

``` r

library(pins)
board <- board_temp(versioned = TRUE)
```

We’re using a temporary board for this demonstration, but in practice,
you might use
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
in a project folder or GitHub repo, or perhaps
[`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md).

Let’s make the `mtcars` dataset available as a JSON file:

``` r

board |> pin_write(mtcars, type = "json")
#> Using `name = 'mtcars'`
#> Creating new version '20260802T194627Z-c2702'
#> Writing to pin 'mtcars'
```

Let’s make a new version of this data by adding a column: `lper100km`,
consumption in liters per 100 km. This could make our data friendlier to
folks outside the United States.

``` r

mtcars_metric <- mtcars
mtcars_metric$lper100km <- 235.215 / mtcars$mpg

board |> pin_write(mtcars_metric, name = "mtcars", type = "json")
#> Creating new version '20260802T194629Z-8416c'
#> Writing to pin 'mtcars'
```

Let’s check our board to ensure we have one pin named `"mtcars"`, with
two versions:

``` r

board |> pin_list()
#> [1] "mtcars"

board |> pin_versions("mtcars")
#> # A tibble: 2 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260802T194627Z-c2702 2026-08-02 19:46:27 c2702
#> 2 20260802T194629Z-8416c 2026-08-02 19:46:29 8416c
```

Because a
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) is
consumed over the web, it doesn’t have access to a file system the way,
for example, a
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
has; we can work around this by creating a manifest file. When a
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) is
set up by a consumer for reading, the pins package uses this file to
discover the pins and their versions. The manifest file is the key to
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)’s
ability to discover pins as if it were a file-system-based board.

After writing pins but *before* publishing, call
[`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md):

``` r

board |> write_board_manifest()
#> Manifest file written to root folder of board, as `_pins.yaml`
```

The maintenance of this manifest file is not automated; it is your
responsibility as the board publisher to keep the manifest up to date.

Let’s confirm that there is a file called `_pins.yaml`:

``` r

withr::with_dir(board$path, fs::dir_ls())
#> _pins.yaml mtcars
```

We can inspect its contents to see each pin in the board, and each
version of each pin:

``` yaml
mtcars:
- mtcars/20260802T194627Z-c2702/
- mtcars/20260802T194629Z-8416c/
```

At this point, we would publish the folder containing the board as a
part of a web site. Let’s pretend that we have served the folder from
our fake website, `https://not.real.website.co/pins/`.

## Consuming

With an up-to-date manifest file, a
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) can
behave as a read-only version of a
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md).
Let’s create a
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
using our fake URL:

``` r

web_board <- board_url("https://not.real.website.co/pins/")
```

The [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
function reads the manifest file to discover the pins and versions:

``` r

web_board |> pin_list()
#> [1] "mtcars"

versions <- web_board |> pin_versions("mtcars")
versions
#> # A tibble: 2 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260802T194627Z-c2702 2026-08-02 19:46:27 c2702
#> 2 20260802T194629Z-8416c 2026-08-02 19:46:29 8416c
```

We can read the most-recent version of the `"mtcars"` pin:

``` r

web_board |> pin_read("mtcars") |> head()
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#>                   lper100km
#> Mazda RX4           11.2007
#> Mazda RX4 Wag       11.2007
#> Datsun 710          10.3164
#> Hornet 4 Drive      10.9914
#> Hornet Sportabout   12.5783
#> Valiant             12.9953
```

We can also read the first version:

``` r

web_board |> pin_read("mtcars", version = versions$version[[1]]) |> head()
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

## Publishing platforms

The goal of this section is to illustrate ways to publish a board as a
part of a website.

### pkgdown

Pins offers another way for package developers to share data associated
with an R package. Publishing a package dataset as a pin can extend your
data’s “audience” to those who have not installed the package.

Using
[pkgdown](https://pkgdown.r-lib.org/articles/customise.html#additional-html-and-files),
any files you save in the directory `pkgdown/assets/` will be copied to
the website’s root directory when
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
is run.

The [*R Packages* book](https://r-pkgs.org/data.html#sec-data-data-raw)
suggests using a folder called `data-raw` for working with datasets;
this can be adapted to use pins. You would start with
`usethis::use_data_raw()`. In a file in your `/data-raw` directory,
wrangle and clean your datasets in the same way as if you were going to
use `usethis::use_data()`. To offer such datasets on a web-based board
instead of as a built-in package dataset, in your `/data-raw` file you
would:

- Create a board:
  `board_folder(here::here("pkgdown/assets/pins-board"))` (you might use
  a different name than `"pins-board"`).
- Write your datasets to the board using
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md).
- At the end, call
  [`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md).

Now when you build your pkgdown site and serve it (perhaps via GitHub
Pages at a URL like `https://user-name.github.io/repo-name/`), your
datasets are available as pins.

The *R Packages* book offers this observation on CRAN and package data:

> Generally, package data should be smaller than a megabyte - if it’s
> larger you’ll need to argue for an exemption.

Publishing a board on your pkgdown site provides a way to offer datasets
too large for CRAN or extended versions of your data. A consumer can
read your pins by setting up a board like:

``` r

board <- board_url("https://user-name.github.io/repo-name/pins-board/")
```

### S3

S3 buckets can be made available to different users using
[permissions](https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-policy.html);
buckets can even be made publicly accessible. Publishing data as a pin
in an S3 bucket can allow your collaborators to read without dealing
with the authentication required by
[`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md).

To offer datasets as a pin on S3 via
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) you
would:

- Create a board: `board_s3("your-existing-bucket")` (set the bucket’s
  permissions to give appropriate people access).
- Write your datasets to the board using
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md).
- At the end, call
  [`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md).

S3 buckets typically have a URL like
`https://your-existing-bucket.s3.us-west-2.amazonaws.com/`. For a person
who has access to your bucket, they can read your pins by setting up a
board like:

``` r

board <- board_url("https://your-existing-bucket.s3.us-west-2.amazonaws.com/")
```
