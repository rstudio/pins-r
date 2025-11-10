# Use a Google Cloud Storage bucket as a board

Pin data to a Google Cloud Storage bucket using the googleCloudStorageR
package.

## Usage

``` r
board_gcs(bucket, prefix = NULL, versioned = TRUE, cache = NULL)
```

## Arguments

- bucket:

  Bucket name. You can only write to an existing bucket, and you can use
  [`googleCloudStorageR::gcs_get_global_bucket()`](https://cloudyr.github.io/googleCloudStorageR//reference/gcs_get_global_bucket.html)
  here.

- prefix:

  Prefix within this bucket that this board will occupy. You can use
  this to maintain multiple independent pin boards within a single GCS
  bucket. Will typically end with `/` to take advantage of Google Cloud
  Storage's directory-like handling.

- versioned:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Authentication

`board_gcs()` is powered by the googleCloudStorageR package which
provides several authentication options, as documented in its [main
vignette](https://code.markedmondson.me/googleCloudStorageR/articles/googleCloudStorageR.html).
The two main options are to create a service account key (a JSON file)
or an authentication token; you can manage either using the
[gargle](https://gargle.r-lib.org/) package.

## Details

- The functions in pins do not create a new bucket. You can create a new
  bucket from R with
  [`googleCloudStorageR::gcs_create_bucket()`](https://cloudyr.github.io/googleCloudStorageR//reference/gcs_create_bucket.html).

- You can pass arguments for
  [googleCloudStorageR::gcs_upload](https://cloudyr.github.io/googleCloudStorageR//reference/gcs_upload.html)
  such as `predefinedAcl` and `upload_type` through the dots of
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md).

- `board_gcs()` is powered by the googleCloudStorageR package, which is
  a suggested dependency of pins (not required for pins in general). If
  you run into errors when deploying content to a server like
  <https://www.shinyapps.io> or
  [Connect](https://posit.co/products/enterprise/connect/), add
  [`requireNamespace("googleCloudStorageR")`](https://code.markedmondson.me/googleCloudStorageR/)
  to your app or document for [automatic dependency
  discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_gcs("pins-testing")
board |> pin_write(mtcars)
board |> pin_read("mtcars")

# A prefix allows you to have multiple independent boards in the same pin.
board_sales <- board_gcs("company-pins", prefix = "sales/")
board_marketing <- board_gcs("company-pins", prefix = "marketing/")
# You can make the hierarchy arbitrarily deep.

# Pass arguments like `predefinedAcl` through the dots of `pin_write`:
board |> pin_write(mtcars, predefinedAcl = "publicRead")
} # }
```
