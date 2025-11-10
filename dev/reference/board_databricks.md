# Use a Databricks Volume as a board

Pin data to a [Databricks
Volume](https://docs.databricks.com/en/sql/language-manual/sql-ref-volumes.html)

## Usage

``` r
board_databricks(
  folder_url,
  host = NULL,
  prefix = NULL,
  versioned = TRUE,
  cache = NULL
)
```

## Arguments

- folder_url:

  The path to the target folder inside Unity Catalog. The path must
  include the catalog, schema, and volume names, preceded by 'Volumes/',
  like `"/Volumes/my-catalog/my-schema/my-volume"`.

- host:

  Your [Workspace Instance
  URL](https://docs.databricks.com/en/workspace/workspace-details.html#workspace-url).
  Defaults to `NULL`. If `NULL`, it will search for this URL in two
  different environment variables, in this order:

  - 'DATABRICKS_HOST'

  - 'CONNECT_DATABRICKS_HOST'

- prefix:

  Prefix within the folder that this board will occupy. You can use this
  to maintain multiple independent pin boards within a single Databricks
  Volume. Make sure to end with '/', to take advantage of Databricks
  Volume directory-like handling.

- versioned:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Authentication

`board_databricks()` searches for an authentication token in three
different places, in this order:

- 'DATABRICKS_TOKEN' environment variable

- 'CONNECT_DATABRICKS_TOKEN' environment variable

- OAuth Databricks token inside the RStudio API

In most cases, the authentication will be a Personal Authentication
Token ('PAT') that is saved as the 'DATABRICKS_TOKEN' environment
variable. To obtain a 'PAT' see: [Databricks personal access token
authentication](https://docs.databricks.com/en/dev-tools/auth/pat.html).

## Details

- The functions in pins do not create a new Databricks Volume.

- `board_databricks()` is powered by the httr2 package, which is a
  suggested dependency of pins (not required for pins in general). If
  you run into errors when deploying content to a server like
  <https://www.shinyapps.io> or
  [Connect](https://posit.co/products/enterprise/connect/), add
  [`requireNamespace("httr2")`](https://httr2.r-lib.org) to your app or
  document for [automatic dependency
  discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_databricks("/Volumes/my-catalog/my-schema/my-volume")
board |> pin_write(mtcars)
board |> pin_read("mtcars")

# A prefix allows you to have multiple independent boards in the same folder.
project_1 <- board_databricks(
  folder_url = "/Volumes/my-catalog/my-schema/my-volume",
  prefix = "project1/"
)
project_2 <- board_databricks(
  folder_url = "/Volumes/my-catalog/my-schema/my-volume",
  prefix = "project2/"
)
} # }
```
