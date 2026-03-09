# Package index

## Pins

- [`pin_read()`](https://pins.rstudio.com/reference/pin_read.md)
  [`pin_write()`](https://pins.rstudio.com/reference/pin_read.md) : Read
  and write objects to and from a board
- [`pin_meta()`](https://pins.rstudio.com/reference/pin_meta.md) :
  Retrieve metadata for a pin
- [`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
  [`pin_upload()`](https://pins.rstudio.com/reference/pin_download.md) :
  Upload and download files to and from a board
- [`pin_versions()`](https://pins.rstudio.com/reference/pin_versions.md)
  [`pin_version_delete()`](https://pins.rstudio.com/reference/pin_versions.md)
  [`pin_versions_prune()`](https://pins.rstudio.com/reference/pin_versions.md)
  : List, delete, and prune pin versions
- [`pin_list()`](https://pins.rstudio.com/reference/pin_list.md) : List
  all pins
- [`pin_search()`](https://pins.rstudio.com/reference/pin_search.md) :
  Search for pins
- [`pin_delete()`](https://pins.rstudio.com/reference/pin_delete.md) :
  Delete a pin
- [`pin_browse()`](https://pins.rstudio.com/reference/pin_browse.md) :
  Browse source of a pin
- [`pin_reactive_read()`](https://pins.rstudio.com/reference/pin_reactive_read.md)
  [`pin_reactive_download()`](https://pins.rstudio.com/reference/pin_reactive_read.md)
  : Wrap a pin in a reactive expression
- [`pin_exists()`](https://pins.rstudio.com/reference/pin_exists.md) :
  Determine if a pin exists

## Boards

Boards abstract over different storage backends, making it easy to share
data in a variety of ways.

- [`board_azure()`](https://pins.rstudio.com/reference/board_azure.md) :
  Use an Azure storage container as a board
- [`board_connect()`](https://pins.rstudio.com/reference/board_connect.md)
  [`board_rsconnect()`](https://pins.rstudio.com/reference/board_connect.md)
  : Use Posit Connect as board
- [`board_connect_url()`](https://pins.rstudio.com/reference/board_connect_url.md)
  [`connect_auth_headers()`](https://pins.rstudio.com/reference/board_connect_url.md)
  : Use a vector of Posit Connect vanity URLs as a board
- [`board_databricks()`](https://pins.rstudio.com/reference/board_databricks.md)
  : Use a Databricks Volume as a board
- [`board_gcs()`](https://pins.rstudio.com/reference/board_gcs.md) : Use
  a Google Cloud Storage bucket as a board
- [`board_gdrive()`](https://pins.rstudio.com/reference/board_gdrive.md)
  : Use a Google Drive folder as a board
- [`board_folder()`](https://pins.rstudio.com/reference/board_folder.md)
  [`board_local()`](https://pins.rstudio.com/reference/board_folder.md)
  [`board_temp()`](https://pins.rstudio.com/reference/board_folder.md) :
  Use a local folder as board
- [`board_ms365()`](https://pins.rstudio.com/reference/board_ms365.md) :
  Use a OneDrive or Sharepoint document library as a board
- [`board_s3()`](https://pins.rstudio.com/reference/board_s3.md) : Use
  an S3 bucket as a board
- [`board_url()`](https://pins.rstudio.com/reference/board_url.md) : Use
  a vector of URLs as a board

## Other functions

- [`cache_browse()`](https://pins.rstudio.com/reference/cache_browse.md)
  [`cache_info()`](https://pins.rstudio.com/reference/cache_browse.md)
  [`cache_prune()`](https://pins.rstudio.com/reference/cache_browse.md)
  : Cache management
- [`board_cache_path()`](https://pins.rstudio.com/reference/board_cache_path.md)
  : Retrieve default cache path
- [`write_board_manifest()`](https://pins.rstudio.com/reference/write_board_manifest.md)
  : Write board manifest file to board's root directory
