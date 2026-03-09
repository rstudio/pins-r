# Use an Azure storage container as a board

Pin data to a container in Azure storage using the AzureStor package.

## Usage

``` r
board_azure(
  container,
  path = "",
  n_processes = 10,
  versioned = TRUE,
  cache = NULL
)
```

## Arguments

- container:

  An azure storage container created by
  [`AzureStor::blob_container()`](https://rdrr.io/pkg/AzureStor/man/blob_container.html)
  or similar.

- path:

  Path to the directory in the container to store pins. Will be created
  if it doesn't already exist. The equivalent of a `prefix` for AWS S3
  storage.

- n_processes:

  Maximum number of processes used for parallel uploads/downloads.

- versioned:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Details

You can create a board in any of the services that AzureStor supports:
blob storage, file storage and Azure Data Lake Storage Gen2 (ADLSgen2).

Blob storage is the classic storage service that is most familiar to
people, but is relatively old and inefficient. ADLSgen2 is a modern
replacement API for working with blobs that is much faster when working
with directories. You should consider using this rather than the classic
blob API where possible; see the examples below.

`board_azure()` is powered by the AzureStor package, which is a
suggested dependency of pins (not required for pins in general). If you
run into errors when deploying content to a server like
<https://www.shinyapps.io> or
[Connect](https://posit.co/products/enterprise/connect/), add
[`requireNamespace("AzureStor")`](https://github.com/Azure/AzureStor) to
your app or document for [automatic dependency
discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (requireNamespace("AzureStor")) {
  # Public access board
  url <- "https://pins.blob.core.windows.net/public-data"
  container <- AzureStor::blob_container(url)
  board <- board_azure(container)
  board |> pin_read("mtcars")
}
#> Loading required namespace: AzureStor
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                     carb
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Datsun 710             1
#> Hornet 4 Drive         1
#> Hornet Sportabout      2
#> Valiant                1
#> Duster 360             4
#> Merc 240D              2
#> Merc 230               2
#> Merc 280               4
#> Merc 280C              4
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 450SLC            3
#> Cadillac Fleetwood     4
#> Lincoln Continental    4
#> Chrysler Imperial      4
#> Fiat 128               1
#> Honda Civic            2
#> Toyota Corolla         1
#> Toyota Corona          1
#> Dodge Challenger       2
#> AMC Javelin            2
#> Camaro Z28             4
#> Pontiac Firebird       2
#> Fiat X1-9              1
#> Porsche 914-2          2
#> Lotus Europa           2
#> Ford Pantera L         4
#> Ferrari Dino           6
#> Maserati Bora          8
#> Volvo 142E             2

if (FALSE) { # \dontrun{
# To create a board that you can write to, you'll need to supply one
# of `key`, `token`, or `sas` to AzureStor::blob_container()
# First, we create a board using the classic Azure blob API
url <- "https://myaccount.blob.core.windows.net/mycontainer"
container <- AzureStor::blob_container(url, sas = "my-sas")
board <- board_azure(container, "path/to/board")
board |> pin_write(iris)

# ADLSgen2 is a modern, efficient way to access blobs
# - Use 'dfs' instead of 'blob' in the account URL to use the ADLSgen2 API
# - Use the 'storage_container' generic instead of the service-specific
#   'blob_container'
# - We reuse the board created via the blob API above
adls_url <- "https://myaccount.dfs.core.windows.net/mycontainer"
container <- AzureStor::storage_container(adls_url, sas = "my-sas")
board <- board_azure(container, "path/to/board")
board |> pin_list()
board |> pin_read("iris")
} # }
```
