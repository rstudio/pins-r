# Use an S3 bucket as a board

Pin data to an S3-compatible storage bucket, such as on Amazon's S3
service, MinIO, or Digital Ocean, using the paws.storage package.

## Usage

``` r
board_s3(
  bucket,
  prefix = NULL,
  versioned = TRUE,
  access_key = NULL,
  secret_access_key = NULL,
  session_token = NULL,
  credential_expiration = NULL,
  profile = NULL,
  region = NULL,
  endpoint = NULL,
  cache = NULL
)
```

## Arguments

- bucket:

  Bucket name. You can only write to an existing bucket.

- prefix:

  Prefix within this bucket that this board will occupy. You can use
  this to maintain multiple independent pin boards within a single S3
  bucket. Will typically end with `/` to take advantage of S3's
  directory-like handling.

- versioned:

  Should this board be registered with support for versions?

- access_key, secret_access_key, session_token, credential_expiration:

  Manually control authentication. See documentation below for details.

- profile:

  Role to use from AWS shared credentials/config file.

- region:

  AWS region. If not specified, will be read from `AWS_REGION`, or AWS
  config file.

- endpoint:

  Endpoint to use; usually generated automatically for AWS from
  `region`. For MinIO and Digital Ocean, use the full URL (including
  scheme like `https://`) of your S3-compatible storage endpoint.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Authentication

`board_s3()` is powered by the paws package which provides a wide range
of authentication options, as documented at
<https://github.com/paws-r/paws/blob/main/docs/credentials.md>. In
brief, there are four main options that are tried in order:

- The `access_key` and `secret_access_key` arguments to this function.
  If you have a temporary session token, you'll also need to supply
  `session_token` and `credential_expiration`. (Not recommended since
  your `secret_access_key` will be recorded in `.Rhistory`)

- The `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` env vars. (And
  `AWS_SESSION_TOKEN` and `AWS_CREDENTIAL_EXPIRATION` env vars if you
  have a temporary session token)

- The AWS shared credential file, `~/.aws/credentials`:

      [profile-name]
      aws_access_key_id=your AWS access key
      aws_secret_access_key=your AWS secret key

  The "default" profile will be used if you don't supply the access key
  and secret access key as described above. Otherwise you can use the
  `profile` argument to use a profile of your choice.

- Automatic authentication from EC2 instance or container IAM role.

See the paws documentation for more unusual options including getting
credentials from a command line process, picking a role when running
inside an EC2 instance, using a role from another profile, and using
multifactor authentication.

## Details

- The functions in pins do not create a new bucket. You can create a new
  bucket from R with
  [paws](https://paws-r.r-universe.dev/paws.storage/reference/s3_create_bucket.html).

- Some functions like
  [`pin_list()`](https://pins.rstudio.com/reference/pin_list.md) will
  work for an S3 board, but don't return useful output.

- You can pass arguments for
  [paws.storage::s3_put_object](https://paws-r.r-universe.dev/paws.storage/reference/s3_put_object.html)
  such as `Tagging` and `ServerSideEncryption` through the dots of
  [`pin_write()`](https://pins.rstudio.com/reference/pin_read.md). (Note
  that these are separate from
  [`pin_write()`](https://pins.rstudio.com/reference/pin_read.md)
  arguments like `tags`.)

- You can use `board_s3()` with S3-compatible object storage on non-AWS
  platforms such as MinIO and Digital Ocean. For this type of object
  storage, use the full URL (including scheme like `https://`) of the
  storage endpoint.

- `board_s3()` is powered by the paws.storage package, which is a
  suggested dependency of pins (not required for pins in general). If
  you run into errors when deploying content to a server like
  <https://www.shinyapps.io> or
  [Connect](https://posit.co/products/enterprise/connect/), add
  [`requireNamespace("paws.storage")`](https://github.com/paws-r/paws)
  to your app or document for [automatic dependency
  discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_s3("pins-test-hadley", region = "us-east-2")
board |> pin_write(mtcars)
board |> pin_read("mtcars")

# A prefix allows you to have multiple independent boards in the same pin.
board_sales <- board_s3("company-pins", prefix = "sales/")
board_marketing <- board_s3("company-pins", prefix = "marketing/")
# You can make the hierarchy arbitrarily deep.

# Pass S3 arguments like `Tagging` through the dots of `pin_write`:
board |> pin_write(mtcars, Tagging = "key1=value1&key2=value2")

} # }
```
