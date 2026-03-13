# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Development Commands

``` r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test(filter = "board_folder")

# Generate documentation from roxygen comments
devtools::document()

# Run R CMD check (CI runs with error-on: "note")
devtools::check()
```

## Package Overview

pins is an R package for publishing data, models, and other R objects to
various storage backends called “boards”. Pins can be versioned and
include metadata. There is also a companion [pins for
Python](https://rstudio.github.io/pins-python/) package with
cross-language compatibility.

## Architecture

### Board System

The core abstraction is the **board** - a storage location for pins. All
boards inherit from `pins_board` and implement S3 methods:

- [`pin_store()`](https://pins.rstudio.com/dev/reference/pin_fetch.md) -
  save a pin to the board
- [`pin_fetch()`](https://pins.rstudio.com/dev/reference/pin_fetch.md) -
  retrieve a pin from the board
- [`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md) -
  get pin metadata
- [`pin_list()`](https://pins.rstudio.com/dev/reference/pin_list.md) -
  list all pins on a board
- [`pin_exists()`](https://pins.rstudio.com/dev/reference/pin_exists.md) -
  check if a pin exists
- [`pin_delete()`](https://pins.rstudio.com/dev/reference/pin_delete.md) -
  remove a pin
- [`pin_versions()`](https://pins.rstudio.com/dev/reference/pin_versions.md) -
  list versions of a pin
- [`pin_version_delete()`](https://pins.rstudio.com/dev/reference/pin_versions.md) -
  delete a specific version

Board implementations in `R/board_*.R`: -
[`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
/
[`board_temp()`](https://pins.rstudio.com/dev/reference/board_folder.md)
/
[`board_local()`](https://pins.rstudio.com/dev/reference/board_folder.md) -
local filesystem -
[`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md) -
Posit Connect -
[`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md) - AWS
S3 (and S3-compatible storage) -
[`board_gcs()`](https://pins.rstudio.com/dev/reference/board_gcs.md) -
Google Cloud Storage -
[`board_azure()`](https://pins.rstudio.com/dev/reference/board_azure.md) -
Azure Blob Storage -
[`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md) -
read-only board from URLs -
[`board_databricks()`](https://pins.rstudio.com/dev/reference/board_databricks.md) -
Databricks Volumes -
[`board_ms365()`](https://pins.rstudio.com/dev/reference/board_ms365.md) -
Microsoft 365 (OneDrive/SharePoint) -
[`board_gdrive()`](https://pins.rstudio.com/dev/reference/board_gdrive.md) -
Google Drive

### API Versions

The package has two API versions: - **v1 (modern)**: Uses
[`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md)/[`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) -
preferred - **v0 (legacy)**: Uses
[`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md)/[`pin()`](https://pins.rstudio.com/dev/reference/pin.md) -
deprecated, files in `R/legacy_*.R`

New board implementations should use
[`new_board_v1()`](https://pins.rstudio.com/dev/reference/new_board.md).

### Pin Types

Supported serialization types: `rds`, `json`, `csv`, `parquet`, `arrow`,
`qs2`

### Testing

Test helpers in `R/testthat.R` provide standard test suites for board
implementations: - `test_api_basic()` - core pin operations -
`test_api_versioning()` - version management - `test_api_meta()` -
metadata handling - `test_api_manifest()` - board manifest files

Board test files call these helpers, e.g.,
`test_api_basic(board_temp())`.

## Key Files

- `R/pin-read-write.R` - main user-facing functions
- `R/board.R` - board base class and utilities
- `R/testthat.R` - shared test infrastructure and error constructors
- `R/meta.R` and `R/pin-meta.R` - metadata handling
- `R/versions.R` and `R/pin_versions.R` - version management
