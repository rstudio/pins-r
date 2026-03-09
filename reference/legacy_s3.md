# S3 board (legacy API)

**\[deprecated\]**

To use an Amazon S3 Storage board, you need an Amazon S3 bucket and a
user with enough permissions to access the S3 bucket. You can sign-up
and create those at <https://aws.amazon.com/>. Note that it can take a
few minutes after you've created it before a bucket is usable.

See [`board_s3()`](https://pins.rstudio.com/reference/board_s3.md) for a
modern version of this legacy board.

## Usage

``` r
legacy_s3(
  bucket = Sys.getenv("AWS_BUCKET"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  cache = NULL,
  region = NULL,
  host = "s3.amazonaws.com",
  name = "s3",
  ...
)

board_register_s3(
  name = "s3",
  bucket = Sys.getenv("AWS_BUCKET"),
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  cache = NULL,
  host = "s3.amazonaws.com",
  region = NULL,
  path = NULL,
  ...
)
```

## Arguments

- bucket:

  The name of the Amazon S3 bucket.

- key, secret:

  The key and secret for your space. You can create a key and secret in
  the "Spaces access keys" in your API settings.

  The `secret` is equivalent to a password, so generally should not be
  stored in your script. The easiest alternative is to store it in the
  `AWS_SECRET_ACCESS_KEY` environment variable, which
  [`board_s3()`](https://pins.rstudio.com/reference/board_s3.md) will
  use by default.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- region:

  The region to use, required in some AWS regions and to enable V4
  signatures.

- host:

  The host to use for storage, defaults to `"s3.amazonaws.com"`.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

- path:

  Subdirectory within `url`

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires an Amazon S3 API key
board <- legacy_s3(bucket = "s3bucket")
} # }
```
