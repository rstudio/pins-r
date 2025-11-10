# Google Cloud board (legacy API)

**\[deprecated\]**

## Usage

``` r
legacy_gcloud(
  bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
  token = NULL,
  cache = NULL,
  name = "gcloud",
  ...
)

board_register_gcloud(
  name = "gcloud",
  bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
  token = NULL,
  cache = NULL,
  path = NULL,
  ...
)
```

## Arguments

- bucket:

  The name of the Google Cloud Storage bucket. Defaults to the
  `GCLOUD_STORAGE_BUCKET` environment variable.

- token:

  The access token of the Google Cloud Storage container. Generally,
  it's best to leave this as `NULL`, and rely on the installed Google
  Cloud SDK to handle authentication.

  If you do want to use an access token, you can retrieve it from
  <https://developers.google.com/oauthplayground>. You will need to
  authorize the "Google Storage API v1" scope.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

- path:

  Subdirectory within `url`

## Details

To use a Google Cloud Storage board, you first need a Google Cloud
Storage account, a Google Storage bucket, and an access token or the
[Google Cloud SDK](https://cloud.google.com/sdk) properly installed and
configured. You can sign-up and create these from
<https://console.cloud.google.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires the Google Cloud SDK to be configured
board <- legacy_gcloud(container = "gcloudcontainer")
} # }
```
