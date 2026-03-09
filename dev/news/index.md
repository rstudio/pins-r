# Changelog

## pins (development version)

## pins 1.4.2

- Added support of the `qs2` format
  ([\#865](https://github.com/rstudio/pins-r/issues/865),
  [@atsyplenkov](https://github.com/atsyplenkov)).

- Removed support for the `qs` format
  ([\#895](https://github.com/rstudio/pins-r/issues/895)).

- Added download progress bar for
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  ([\#872](https://github.com/rstudio/pins-r/issues/872),
  [@lbm364dl](https://github.com/lbm364dl)).

- Added support for writing pins with multiple types, like
  `type = c("rds", "csv")`
  ([\#877](https://github.com/rstudio/pins-r/issues/877),
  [@lbm364dl](https://github.com/lbm364dl)).

- Switched to using the native pipe in examples and documentation
  ([\#879](https://github.com/rstudio/pins-r/issues/879)).

- Further escalated gradual deprecation process for legacy pins
  functions such as
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md)
  ([\#879](https://github.com/rstudio/pins-r/issues/879)).

- Added support for
  [`pin_list()`](https://pins.rstudio.com/dev/reference/pin_list.md) to
  [`board_gcs()`](https://pins.rstudio.com/dev/reference/board_gcs.md)
  for Google Cloud Storage
  ([\#889](https://github.com/rstudio/pins-r/issues/889),
  [@sverrirarnors](https://github.com/sverrirarnors)).

## pins 1.4.1

CRAN release: 2025-04-30

- Support new `preview_data` parameter for pin previews on Posit Connect
  ([\#850](https://github.com/rstudio/pins-r/issues/850)).

- Fixed a bug in how
  [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  handles pin names
  ([\#852](https://github.com/rstudio/pins-r/issues/852)).

- Improved documentation for S3-compatible object storage
  ([\#853](https://github.com/rstudio/pins-r/issues/853)).

- Prepend ‘<https://>’ to Databricks host if missing
  ([\#855](https://github.com/rstudio/pins-r/issues/855)).

- Fixed handling of
  [`rsconnect::accounts()`](https://rstudio.github.io/rsconnect/reference/accounts.html)
  results ([\#861](https://github.com/rstudio/pins-r/issues/861)).

- Escalated gradual deprecation process for legacy pins functions such
  as [`pin()`](https://pins.rstudio.com/dev/reference/pin.md)
  ([\#864](https://github.com/rstudio/pins-r/issues/864)).

## pins 1.4.0

CRAN release: 2024-10-07

### Lifecycle changes

- Changed the function signature of
  [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  to be consistent with
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  i.e. arguments like `tags` must be passed by name and not position
  ([\#809](https://github.com/rstudio/pins-r/issues/809)).

- Started gradual deprecation process for legacy pins functions such as
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md)
  ([\#844](https://github.com/rstudio/pins-r/issues/844)).

### Other improvements

- Added example Python code to pin previews for Posit Connect
  ([\#806](https://github.com/rstudio/pins-r/issues/806)).

- Fixed a bug in how pins with the same name but different owners on
  Posit Connect were identified
  ([\#808](https://github.com/rstudio/pins-r/issues/808)).

- Fixed a bug in handling folders with duplicate names for Google Drive
  ([\#819](https://github.com/rstudio/pins-r/issues/819),
  [@UchidaMizuki](https://github.com/UchidaMizuki)).

- Fixed how previously deleted pin versions are detected
  ([\#838](https://github.com/rstudio/pins-r/issues/838),
  [@MichalLauer](https://github.com/MichalLauer)).

- Added new board for Databricks Volumes
  [`board_databricks()`](https://pins.rstudio.com/dev/reference/board_databricks.md)
  ([\#839](https://github.com/rstudio/pins-r/issues/839),
  [@edgararuiz](https://github.com/edgararuiz)).

- Switched writing with `type = "parquet"` to use the nanoparquet
  package ([\#843](https://github.com/rstudio/pins-r/issues/843)).

## pins 1.3.0

CRAN release: 2023-11-09

### Breaking changes

- Changed the function signature of
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) so
  arguments like `type` and `title` must be passed by name and not
  position ([\#792](https://github.com/rstudio/pins-r/issues/792)).

### Other improvements

- Removed content and user caches for Connect altogether. Now, we look
  up usernames and content on the Connect server every time
  ([\#793](https://github.com/rstudio/pins-r/issues/793)).

- Added new `urls` item to metadata for a pin
  ([\#795](https://github.com/rstudio/pins-r/issues/795)).

## pins 1.2.2

CRAN release: 2023-09-09

- Fixed how dots are checked in
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) to
  make user-facing messages more clear
  ([\#770](https://github.com/rstudio/pins-r/issues/770)).

- Improved documentation about Connect caches
  ([\#771](https://github.com/rstudio/pins-r/issues/771)) and deleting
  pin versions ([\#773](https://github.com/rstudio/pins-r/issues/773)).

- Added `board_deparse` for
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  ([\#774](https://github.com/rstudio/pins-r/issues/774)).

- Fixed how
  [`board_gdrive()`](https://pins.rstudio.com/dev/reference/board_gdrive.md)
  handles dribble objects
  ([\#780](https://github.com/rstudio/pins-r/issues/780),
  [@gorkang](https://github.com/gorkang) and
  [\#782](https://github.com/rstudio/pins-r/issues/782)).

## pins 1.2.1

CRAN release: 2023-08-16

- New environment variable `PINS_CACHE_DIR` controls the location of the
  default cache path
  ([\#748](https://github.com/rstudio/pins-r/issues/748)).

- Added new board for Google Drive
  [`board_gdrive()`](https://pins.rstudio.com/dev/reference/board_gdrive.md)
  ([\#749](https://github.com/rstudio/pins-r/issues/749)).

- Updated test for new arrow release
  ([\#764](https://github.com/rstudio/pins-r/issues/764)).

## pins 1.2.0

CRAN release: 2023-05-18

### Breaking changes

- [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md) no
  longer writes identical pin contents by default, and gains a
  `force_identical_write` argument for writing even when the pin
  contents are identical to the last version
  ([\#735](https://github.com/rstudio/pins-r/issues/735)).

### Other improvements

- The `print` method for boards no longer calls
  [`pin_list()`](https://pins.rstudio.com/dev/reference/pin_list.md)
  internally ([\#718](https://github.com/rstudio/pins-r/issues/718)).

- [`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md) now
  uses pagination for listing and versioning
  ([\#719](https://github.com/rstudio/pins-r/issues/719),
  [@mzorko](https://github.com/mzorko)).

- Added `type = "parquet"` to read and write Parquet files
  ([\#729](https://github.com/rstudio/pins-r/issues/729)).

- Updated error messages and type checking
  ([\#731](https://github.com/rstudio/pins-r/issues/731)) along with
  testing strategy
  ([\#724](https://github.com/rstudio/pins-r/issues/724)).

- Added new check for whether a new version is the same as the previous
  version, as can happen when writing pin versions very quickly
  ([\#727](https://github.com/rstudio/pins-r/issues/727)).

- Added new `headers` argument for
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md),
  mostly for authentication, as well as new board for Connect vanity
  URLs
  [`board_connect_url()`](https://pins.rstudio.com/dev/reference/board_connect_url.md)
  ([\#732](https://github.com/rstudio/pins-r/issues/732)).

- Fixed bug in
  [`cache_prune()`](https://pins.rstudio.com/dev/reference/cache_browse.md)
  to correctly find caches for
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  ([\#734](https://github.com/rstudio/pins-r/issues/734)).

## pins 1.1.0

CRAN release: 2023-01-21

### Breaking changes

- Change the function
  [`board_rsconnect()`](https://pins.rstudio.com/dev/reference/board_connect.md)
  to
  [`board_connect()`](https://pins.rstudio.com/dev/reference/board_connect.md),
  following RStudio’s rebranding to Posit
  ([\#689](https://github.com/rstudio/pins-r/issues/689)).

- Changed `type = "csv"` to use R’s default value for `stringsAsFactors`
  i.e. `FALSE` ([\#664](https://github.com/rstudio/pins-r/issues/664)).

- Functions for viewing legacy API pins in the RStudio Viewer pane are
  now deprecated (when possible) or removed
  ([\#679](https://github.com/rstudio/pins-r/issues/679)).

- The functions for accessing Kaggle resource as pins are no longer
  supported. We recommend you use the Kaggle CLI instead
  ([\#698](https://github.com/rstudio/pins-r/issues/698)).

### Other improvements

- Added vignettes describing how to manage custom formats and web-based
  boards ([\#631](https://github.com/rstudio/pins-r/issues/631),
  [\#685](https://github.com/rstudio/pins-r/issues/685),
  [@ijlyttle](https://github.com/ijlyttle)).

- Added new board for Google Cloud Storage
  [`board_gcs()`](https://pins.rstudio.com/dev/reference/board_gcs.md)
  ([\#695](https://github.com/rstudio/pins-r/issues/695)).

- Added new `tags` item to metadata for a pin
  ([\#677](https://github.com/rstudio/pins-r/issues/677)).

- Improved error message for
  [`pin_versions()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
  ([\#657](https://github.com/rstudio/pins-r/issues/657)).

- Switched content and user caches for Connect to use environments
  instead of files on disk. This means caches will no longer persist
  between sessions but will be much less likely to end up in a broken
  state ([\#667](https://github.com/rstudio/pins-r/issues/667)).

- Added
  [`write_board_manifest()`](https://pins.rstudio.com/dev/reference/write_board_manifest.md)
  to write a manifest file `_pins.yaml` recording all pins and their
  versions to the board’s root directory. This function only works for
  boards that are not read-only
  ([\#661](https://github.com/rstudio/pins-r/issues/661), based on work
  of [@ijlyttle](https://github.com/ijlyttle)).

- Updated
  [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  to handle versions recorded via a manifest file
  ([\#681](https://github.com/rstudio/pins-r/issues/681), based on work
  of [@ijlyttle](https://github.com/ijlyttle)).

- Updated code preview on Posit Connect
  ([\#690](https://github.com/rstudio/pins-r/issues/690)).

## pins 1.0.3

CRAN release: 2022-09-24

- The `arrow` package is now suggested, rather than imported
  ([\#644](https://github.com/rstudio/pins-r/issues/644),
  [@jonthegeek](https://github.com/jonthegeek)).

- Fixed how Connect usernames are handled in messages, preview, etc
  ([\#643](https://github.com/rstudio/pins-r/issues/643)).

- Increased datetime precision to the second, for
  [`pin_versions()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
  and related functions
  ([\#642](https://github.com/rstudio/pins-r/issues/642),
  [@tomsing1](https://github.com/tomsing1)).

- Pass the dots from
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  through to `s3_upload_file()` and `s3_uploade_yaml()` to support S3
  tagging, encryption options, etc for pins
  ([\#648](https://github.com/rstudio/pins-r/issues/648),
  [\#652](https://github.com/rstudio/pins-r/issues/652),
  [@fh-mthomson](https://github.com/fh-mthomson)).

## pins 1.0.2

CRAN release: 2022-08-23

- [`board_rsconnect()`](https://pins.rstudio.com/dev/reference/board_connect.md)
  now correctly finds the created date for pins
  ([\#623](https://github.com/rstudio/pins-r/issues/623),
  [@bjfletcher](https://github.com/bjfletcher)).

- [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  now better handles path expansion
  ([\#585](https://github.com/rstudio/pins-r/issues/585),
  [@sellorm](https://github.com/sellorm)).

- The `pin_reactive_*()` functions now use the hash (rather than the
  created date) for polling
  ([\#595](https://github.com/rstudio/pins-r/issues/595),
  [@thomaszwagerman](https://github.com/thomaszwagerman)).

## pins 1.0.1

CRAN release: 2021-12-15

- [`board_azure()`](https://pins.rstudio.com/dev/reference/board_azure.md)
  now allows you to set a `path` so that multiple boards can share the
  same container ([\#528](https://github.com/rstudio/pins-r/issues/528),
  [@hongooi73](https://github.com/hongooi73)).

- [`board_deparse()`](https://pins.rstudio.com/dev/reference/board_deparse.md)
  is more likely to generate runnable code when used with
  [`board_rsconnect()`](https://pins.rstudio.com/dev/reference/board_connect.md)
  ([\#553](https://github.com/rstudio/pins-r/issues/553)).

- `legazy_azure()` works once again
  ([\#527](https://github.com/rstudio/pins-r/issues/527)).

- [`legacy_github()`](https://pins.rstudio.com/dev/reference/legacy_github.md)
  works once again
  ([\#549](https://github.com/rstudio/pins-r/issues/549)).

- [`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md) now
  includes pin `name`
  ([\#544](https://github.com/rstudio/pins-r/issues/544)).

- [`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md)
  works better when called directly, due to standardisation of cache
  paths all computation
  ([\#529](https://github.com/rstudio/pins-r/issues/529)).

- Drop add-ins since they’re not tested or documented
  ([\#525](https://github.com/rstudio/pins-r/issues/525))

## pins 1.0.0

CRAN release: 2021-10-02

pins 1.0.0 includes a new, more explicit, API that includes robust
support for versioning. In the modern API, you create a board object
which is passed to every `pin_` function instead of “registering” a
board that is later refereed to with a string. This leads to code like
this:

``` r
board <- board_local()
board %>% pin_write(mtcars, "mtcars")
board %>% pin_read("mtcars")
```

The legacy API
([`pin()`](https://pins.rstudio.com/dev/reference/pin.md),
[`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md), and
[`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md))
will continue to work, but new features will only be implemented with
the new API, so we encourage you to switch to the modern API as quickly
as possible. Learn more in
[`vignette("pins-update")`](https://pins.rstudio.com/dev/articles/pins-update.md).

### Modern pin functions

- [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  replace most uses of
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) and
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md).
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  has a `type` argument that allows you to choose how to serialise your
  R objects to disk, allowing you to manage the tradeoffs between speed,
  generality, and language inter-op, and a `metadata` argument that
  allows you to store arbitrary metadata
  ([\#430](https://github.com/rstudio/pins-r/issues/430)).

- [`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  and
  [`pin_upload()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  are lower-level versions of
  [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md) and
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  that work with file paths rather than R objects. They replace the use
  of [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) with a
  path and eliminate the type-instability in
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md),
  which can return either an R object or a character vector of paths.

- [`pin_browse()`](https://pins.rstudio.com/dev/reference/pin_browse.md)
  replaces
  [`board_browse()`](https://pins.rstudio.com/dev/reference/custom-boards.md),
  and takes you to a specific pin, either the original source on the
  internet, or the cached version on your local file system
  ([\#435](https://github.com/rstudio/pins-r/issues/435)).

- [`pin_delete()`](https://pins.rstudio.com/dev/reference/pin_delete.md)
  replaces
  [`pin_remove()`](https://pins.rstudio.com/dev/reference/pin_remove.md),
  and can delete multiple pins
  ([\#433](https://github.com/rstudio/pins-r/issues/433)).

- [`pin_list()`](https://pins.rstudio.com/dev/reference/pin_list.md)
  lists all pins in a board.

- [`pin_meta()`](https://pins.rstudio.com/dev/reference/pin_meta.md)
  replaces
  [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md) and
  retrieves pin metadata
  ([\#418](https://github.com/rstudio/pins-r/issues/418)).

- [`pin_search()`](https://pins.rstudio.com/dev/reference/pin_search.md)
  replaces
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md). It
  is much more limited because the previous version was based on
  assumptions that are not true for many boards.

- [`pin_reactive_read()`](https://pins.rstudio.com/dev/reference/pin_reactive_read.md)
  and
  [`pin_reactive_download()`](https://pins.rstudio.com/dev/reference/pin_reactive_read.md)
  replace
  [`pin_reactive()`](https://pins.rstudio.com/dev/reference/pin_reactive.md).

- [`pin_exists()`](https://pins.rstudio.com/dev/reference/pin_exists.md)
  reports whether or not a pin exists.

- [`pin_version_delete()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
  allows you to delete a single version.
  [`pin_versions_prune()`](https://pins.rstudio.com/dev/reference/pin_versions.md)
  ([\#459](https://github.com/rstudio/pins-r/issues/459)) allows you to
  easily prune old versions keeping either a specified number of
  versions, or all versions beneath a certain age.

### Modern boards

This version includes the following modern boards:

- [`board_azure()`](https://pins.rstudio.com/dev/reference/board_azure.md)
  stores data in Azure’s blob storage. It is built on top of
  [AzureStor](https://github.com/Azure/AzureStor)
  ([\#474](https://github.com/rstudio/pins-r/issues/474)).

- [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
  is a generalised replacement for the legacy local board.
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
  can store data in any directory, making it possible to share boards
  using shared network drives or on dropbox or similar. If you using
  pins casually and don’t want to pick a directory,
  [`board_local()`](https://pins.rstudio.com/dev/reference/board_folder.md)
  is a variant of
  [`board_folder()`](https://pins.rstudio.com/dev/reference/board_folder.md)
  that stores data in a system data directory.

- [`board_kaggle_dataset()`](https://pins.rstudio.com/dev/reference/board_kaggle.md)
  and `board_kaggle_competition()` allow you to download data from
  Kaggle. The data is automatically cached so that it’s only downloaded
  when it changes.

- [`board_ms365()`](https://pins.rstudio.com/dev/reference/board_ms365.md)
  allow to pin data to MS One Drive and Sharpoint
  ([\#498](https://github.com/rstudio/pins-r/issues/498),
  [@hongooi73](https://github.com/hongooi73)).

- [`board_rsconnect()`](https://pins.rstudio.com/dev/reference/board_connect.md)
  shares data on [RStudio
  connect](https://posit.co/products/enterprise/connect/). This board
  supports both modern and legacy APIs, so that you and your colleagues
  can use a mixture of pins versions as you transition to pins 1.0.0.
  Note that the compatibility is one directional: you can
  [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  pins created by
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md), but you
  can’t [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md)
  pins created by
  [`pin_write()`](https://pins.rstudio.com/dev/reference/pin_read.md).

- [`board_s3()`](https://pins.rstudio.com/dev/reference/board_s3.md)
  stores data in Amazon’s S3 service. It is built on top of paws.

- [`board_url()`](https://pins.rstudio.com/dev/reference/board_url.md)
  lets you create a manual board from a vector of URLs. This is useful
  because
  [`pin_download()`](https://pins.rstudio.com/dev/reference/pin_download.md)
  and [`pin_read()`](https://pins.rstudio.com/dev/reference/pin_read.md)
  are cached, so they only re-download the data if it has changed since
  the last time you used it
  ([\#409](https://github.com/rstudio/pins-r/issues/409)). This board is
  a replacement for
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md)’s ability to
  work directly with URLs

The legacy boards will continue to work with the legacy pins API; we
will implement modern versions of the remaining legacy boards based on
user feedback.

### Minor improvements and bug fixes

- All board objects now have class beginning with `pins_board_` and also
  inherit from common superclass `pins_board`.

- Pins no longer works with the connections pane. This automatically
  registered code tended to be either dangerous (because it’s easy to
  accidentally leak credentials) or useless (because it relied on
  variables that the connection pane doesn’t capture).

- Pinned data frames are longer converted to tibbles.

- The “packages” board is no longer registered by default; if you want
  to use this you’ll need to register with `board_register("packages")`.
  It has been radically simplified so that it will no longer download
  packages, and it
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) now
  searches all packages that you have installed, rather than a stale
  snapshot of data in CRAN packages. The CRAN files dataset has been
  removed from the package.

- [`board_browse()`](https://pins.rstudio.com/dev/reference/custom-boards.md)
  now works with local boards.

- [`board_rsconnect()`](https://pins.rstudio.com/dev/reference/board_connect.md)
  will automatically connect to the current RSC pin board when run
  inside RSC itself (assuming you have version 1.8.8 or later)
  ([\#396](https://github.com/rstudio/pins-r/issues/396)).

- [`cache_browse()`](https://pins.rstudio.com/dev/reference/cache_browse.md),
  [`cache_info()`](https://pins.rstudio.com/dev/reference/cache_browse.md),
  and
  [`cache_prune()`](https://pins.rstudio.com/dev/reference/cache_browse.md)
  provide some basic tooling around the local pins cache maintained by
  pins ([\#438](https://github.com/rstudio/pins-r/issues/438)).

- [`pin_fetch()`](https://pins.rstudio.com/dev/reference/pin_fetch.md)
  has been removed

- `option(pins.invisible)` is now defunct and ignored.

- You can no longer switch from a versioned pin to an unversioned pin
  without first deleting the pin
  ([\#410](https://github.com/rstudio/pins-r/issues/410)).

## pins 0.4.5

CRAN release: 2021-01-05

### Pins

- Support downloading remote files when service returns incompatible
  `data.txt` file
  ([\#310](https://github.com/rstudio/pins-r/issues/310)).

- Support for pins over 100MB in Windows systems
  ([\#313](https://github.com/rstudio/pins-r/issues/313)).

- Avoid Windows crashing in
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) under some
  locales ([\#127](https://github.com/rstudio/pins-r/issues/127)).

### Boards

- Silenced ‘no encoding supplied’ warning
  ([\#330](https://github.com/rstudio/pins-r/issues/330)).

### Local

- [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) no
  longer searches text with an undocumented regular expression syntax
  ([\#270](https://github.com/rstudio/pins-r/issues/270)).

### S3

- Default to using HTTPS in S3 boards
  ([\#304](https://github.com/rstudio/pins-r/issues/304)).

- Support for AWS V4 signatures when registering S3 boards with `region`
  parameter ([\#304](https://github.com/rstudio/pins-r/issues/304))

### Cloud

- Support for `path` to register a board under a subpath for Azure,
  DigitalOcean, Google Cloud and S3 boards
  ([\#200](https://github.com/rstudio/pins-r/issues/200)).

- Avoid creating pins named with unsupported characters for Azure,
  DigitalOcean, Google Cloud and S3 boards
  ([\#193](https://github.com/rstudio/pins-r/issues/193)).

### GitHub

- Properly store relative paths when `path` parameter is specified in
  GitHub boards ([\#199](https://github.com/rstudio/pins-r/issues/199)).

- Add support for repos with a ‘main’ branch as default
  ([\#336](https://github.com/rstudio/pins-r/issues/336)).

- Add support for large file in private repo releases
  ([\#292](https://github.com/rstudio/pins-r/issues/292)).

- When a board is registered with `versions = FALSE`, GitHub tags are
  also delete when large files are present
  ([\#285](https://github.com/rstudio/pins-r/issues/285)).

### RStudio Connect

- Invalid ‘account’ or ‘server’ parameters show proper errors
  ([\#296](https://github.com/rstudio/pins-r/issues/296)).

- Increase total entries retrieved with
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md),
  configurable with `pins.search.count`
  ([\#296](https://github.com/rstudio/pins-r/issues/296)).

- Fix regression introduced in pins 0.4.2
  ([\#253](https://github.com/rstudio/pins-r/issues/253)) preventing
  users from collaborating on existing pins they have access to
  ([\#302](https://github.com/rstudio/pins-r/issues/302)).

- Avoid deleting pin when upload fails to avoid deleting versions
  ([\#306](https://github.com/rstudio/pins-r/issues/306)).

- Support re-creating pins from pins not previously properly updated
  ([\#308](https://github.com/rstudio/pins-r/issues/308)).

- Adjust pin preview to only display 1K rows instead of 10K
  ([\#315](https://github.com/rstudio/pins-r/issues/315)).

- Avoid changing columns names on data frame preview
  ([\#190](https://github.com/rstudio/pins-r/issues/190)).

- Improve error message when token authentication fails
  ([\#327](https://github.com/rstudio/pins-r/issues/327)).

## pins 0.4.4

CRAN release: 2020-10-30

### Pins

- For files bigger than 100MB
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) creates
  symlinks to speed up uploads, can be configured using the
  `pins.link.size` option
  ([\#273](https://github.com/rstudio/pins-r/issues/273)).

- When using `pin(zip = TRUE)` the zip no longer contains local patahs
  ([\#277](https://github.com/rstudio/pins-r/issues/277)).

### Google Cloud

- Disable caching on `data.txt` to support creating multiple pins at
  once ([\#275](https://github.com/rstudio/pins-r/issues/275)).

### RStudio

- Prevent connections pane from hanging when multiple pins are updated
  at once ([\#280](https://github.com/rstudio/pins-r/issues/280)).

### Website

- Support for `pin_get(download = FALSE)` to avoid checking for updates.

### RStudio Connect

- Support for servers with mismatched `http` vs `https` protocols.

- Make use of `RSCONNECT_TAR` when running a report inside RStudio
  Connect ([\#293](https://github.com/rstudio/pins-r/issues/293)).

## pins 0.4.3

CRAN release: 2020-07-10

### Boards

- Properly export `board_pin_versions` to allow custom boards extending
  versions ([\#265](https://github.com/rstudio/pins-r/issues/265)).

### Website

- Fix regression creating pins when using a brand new cloud board
  ([\#268](https://github.com/rstudio/pins-r/issues/268)).

## pins 0.4.2

CRAN release: 2020-07-05

### Website

- Fix issue removing pins with custom domain names from cloud boards
  ([\#234](https://github.com/rstudio/pins-r/issues/234)).

- Fix warning when using
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) against
  storage locations with custom domain name
  ([\#237](https://github.com/rstudio/pins-r/issues/237)).

- Fix issue where datatxt was not refreshing deleted entries
  ([\#239](https://github.com/rstudio/pins-r/issues/239)).

### RStudio Connect

- Support for `versions = FALSE` in
  [`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md)
  to avoid using too much space when creating pins
  ([\#245](https://github.com/rstudio/pins-r/issues/245)).

- Prevent administrators from overriding pins they don’t own, unless the
  pin is specified as `user/name`
  ([\#253](https://github.com/rstudio/pins-r/issues/253)).

- Support to connect to servers that have a redirect configured when the
  full server URL is not specified in
  [`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md)
  ([\#256](https://github.com/rstudio/pins-r/issues/256)).

- Throw error when multiple accounts are associated to the same server
  ([\#261](https://github.com/rstudio/pins-r/issues/261)).

## pins 0.4.1

CRAN release: 2020-05-28

### Pin

- When running in production environments (which usually set the
  `R_CONFIG_ACTIVE` environment variable), avoid using shared caches.

- Fix [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) failing
  to update cache when server returns `NULL` etag.

- Support for `custom_metadata` in
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) to allow
  saving custom fields in `data.txt` file.

- Improve performannce for
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) from URLs
  containing large files that are already been cached prerviously by
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md)
  ([\#225](https://github.com/rstudio/pins-r/issues/225)).

- Avoid showing upload or download progress when creating R Markdown
  documents and other non-interactive use cases
  ([\#227](https://github.com/rstudio/pins-r/issues/227)).

- When pin(url) fails and local cache exists, produce warning and
  retrieve cached version
  ([\#231](https://github.com/rstudio/pins-r/issues/231)).

- Support for `pin(zip = TRUE)` to create a zip file of the given path
  before creating the pin
  ([\#232](https://github.com/rstudio/pins-r/issues/232)).

### RStudio Connect

- Fix when overriding pin with corrupt metadata.

- Avoid using shared caches when running inside RStudio Connect.

- Fixed ‘invalid uid’ warning when creaating pin undner some Linux
  servers ([\#263](https://github.com/rstudio/pins-r/issues/263)).

### Kaggle

- Support to find and download competition datasets.

## pins 0.4.0

CRAN release: 2020-04-07

- Support for versioning in all boards.

- Support for DigitalOcean board.

### Pin

- Finding pins with
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md)
  sort results by default
  ([\#201](https://github.com/rstudio/pins-r/issues/201)).

- Avoid `incomplete final line found` warning error wheen reading
  manifests.

- Support for using
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) across
  multiple concurrent processes
  ([\#182](https://github.com/rstudio/pins-r/issues/182)).

- Support in
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) to
  download arbitrary files from cloud boards like Azure, DigitalOcean,
  GitHub, Google Cloud, RStudio Connect, and S3.

- Fix issue where http HEAD requests could tgimeout and prevent pin from
  downloading in very slow connections.

### RStudio

- Support `access_type` parameter for RStudio Connect.

- [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) now refreshes
  the connections pane.

- [`pin_remove()`](https://pins.rstudio.com/dev/reference/pin_remove.md)
  now refreshes the connections pane.

### RStudio Connect

- Support for `code` parameter in
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) to customize
  R code used in the UI to retrieve the pin
  ([\#77](https://github.com/rstudio/pins-r/issues/77)).

- Improve error message for
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md) with
  duplicate names
  ([\#171](https://github.com/rstudio/pins-r/issues/171)).

- Fix board register error when using URL with ports
  ([\#195](https://github.com/rstudio/pins-r/issues/195)).

- Enable retrieving public pins without authentication
  ([\#83](https://github.com/rstudio/pins-r/issues/83)).

### GitHub

- Support for `versions = FALSE` in
  [`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md)
  to also delete release files when pin is removed
  ([\#91](https://github.com/rstudio/pins-r/issues/91)).

- Support for `versions = FALSE` in
  [`board_register()`](https://pins.rstudio.com/dev/reference/board_register.md)
  to avoid creating versioned GitHub releases
  ([\#197](https://github.com/rstudio/pins-r/issues/197)).

- Support for committing all github files with a single commit
  ([\#197](https://github.com/rstudio/pins-r/issues/197)).

- Support for custom GitHub hosts including GitHub Enterprise using the
  `host` parameter in
  [`board_register_github()`](https://pins.rstudio.com/dev/reference/legacy_github.md)
  ([\#163](https://github.com/rstudio/pins-r/issues/163)).

### Websites

- Using [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) now
  searches for `data.txt` files in URLs when the URL contains no file
  extension, behavior can be turnned off with the `pins.search.datatxt`
  option.

## pins 0.3.2

CRAN release: 2020-02-06

### Pins

- [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md)
  adds support for `metadata` parameter to avoid retrieving pin
  contents.

### S3

- Added support for `host` parameter to configure “s3.amazonaws.com” to
  custom locations.

### GitHub

- Fix regression uploading large datasets as release files.

## pins 0.3.1

CRAN release: 2020-01-10

### Pins

- [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md)
  prints long character strings in their own line.

- Fixed issue in
  [`pin_remove()`](https://pins.rstudio.com/dev/reference/pin_remove.md)
  for S3, Azure, GCloud, Google and website boards
  ([\#138](https://github.com/rstudio/pins-r/issues/138)).

- Fixed issue when pinning complex data frames with nested lists
  ([\#142](https://github.com/rstudio/pins-r/issues/142)).

### Azure

- Added missing `key` parameter in
  [`board_register_azure()`](https://pins.rstudio.com/dev/reference/legacy_azure.md).

### RStudio

- Fixed connection suggested code when caused by
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md)
  ([\#137](https://github.com/rstudio/pins-r/issues/137)).

- Fixed connection launcher for Azure connections.

### RStudio Connect

- Fix issue uploading large pins causing
  `is.character(type) is not TRUE` error.

- Fix issue affecting boards registered with trailing slash
  ([\#151](https://github.com/rstudio/pins-r/issues/151)).

- Improve error messages when a pin fails to be created
  ([\#149](https://github.com/rstudio/pins-r/issues/149)).

- Added support for `CONNECT_API_KEY` and `CONNECT_SERVER` in place of
  `RSCONNECT_API` and `RSCONNECT_SERVER`, which they are still supported
  for backwards-compatibility.

## pins 0.3.0

CRAN release: 2019-11-26

- Support for Azure board.

- Support for Google Cloud board.

- Support for S3 board.

### Pins

- [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) extracts URLs
  that end in `.gz` and `zip`, which can be disabled with
  `pin("<url>", extract = FALSE)`.

- [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) no longer
  prints the pin, you can revert this behavior with
  `options(pins.invisible = FALSE)`
  ([\#122](https://github.com/rstudio/pins-r/issues/122)).

- Show upload progress for files larger than 10mb.

- Avoid changing `data.table` when using
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) and
  `get_pin()`.

- Support for
  [`pin_info()`](https://pins.rstudio.com/dev/reference/pin_info.md) to
  describe a pin and `extended = TRUE` in
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) to
  show all extended information about each pin.

- Support for `extract` parameter in
  [`pin_get()`](https://pins.rstudio.com/dev/reference/pin_get.md),
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md), and
  [`pin_reactive()`](https://pins.rstudio.com/dev/reference/pin_reactive.md)
  to override default file extraction behavior.

- Support to extract gzip files when the `R.utils` package is installed.

- Show download progress for files larger than 10mb.

- Support for [`I()`](https://rdrr.io/r/base/AsIs.html) to force a pin
  to be created without additional overhead of exporting CSV and other
  enhancements ([\#73](https://github.com/rstudio/pins-r/issues/73)).

- Support creating automatic `name` when using
  [`pin()`](https://pins.rstudio.com/dev/reference/pin.md) and multiple
  URL.

### Boards

- Avoid “cannot create dir” warning in systems with an empty cache
  folder.

### RStudio

- Gracefully handle errors when a pin’s preview can’t be created.

- Website boards now support the browse menu item in the connection.

- Fix data frames previewing character columns with special characters.

- Fix connection code when a board is registered automatically.

### RStudio Connect

- Fix issue where
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md)
  would not show all available pins.

- Fix issue where RStudio Connections pane would not show all pins.

- Store all downloaded content under user subfolder insited rsconnect
  cache.

- Removed
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md)
  workaround for beta rsconnect server where searching was causing
  timeout.

- Fix for RStudio Connect servers where `/content/` is not used in
  content URLs.

- Fix issue expanding pins columns in RStudio for pins with similar
  names in RStudio Connect boards.

- Fix issue removing pins with similar names in RStudio Connect boards.

### Python

- Support for Python 3.

### Websites

- The `name` parameter in
  [`board_register_datatxt()`](https://pins.rstudio.com/dev/reference/legacy_datatxt.md)
  is now optional.

- Fix in
  [`pin_find()`](https://pins.rstudio.com/dev/reference/pin_find.md) to
  properly search desecription files.

### GitHub

- Various fixes for GitHub boards using the `path` parameter when
  registering the board
  ([\#121](https://github.com/rstudio/pins-r/issues/121)).

## pins 0.2.0

CRAN release: 2019-10-01

### RStudio Connect

- Support for retrieving pins shared by others in RStudio Connect
  boards.

- Support for RStudio Connect servers running under a subpath in the
  server.

- Add support for `RSCONNECT_SERVER` environment variable to ease
  configuration of automated RStudio Connect reports.

- Fix intermittent failure to retrieve pins from RStudio Connect boards
  while creating them.

- Fix in RStudio Connect boards to retrieve pins that match other pin
  names ([\#45](https://github.com/rstudio/pins-r/issues/45)).

- Fix for data frames with nested data frames in rsconnect boards
  ([\#36](https://github.com/rstudio/pins-r/issues/36)).

### GitHub

- [`board_register_github()`](https://pins.rstudio.com/dev/reference/legacy_github.md)
  now checks for the repo to exist
  ([\#63](https://github.com/rstudio/pins-r/issues/63)).

- Adjusted max upload file to 25mb to avoid “server error” in the API,
  larger files than 25mb uploaded as release files. This can be
  configured using the `pins.github.release` option, which deefaults to
  25.

- Allow overriding GitHub pin over a pin that partially failed to be
  created.

### Boards

- Using a board will attempt to automatically register, such that
  `pin(iris, board = "rsconnect")` would work for the default
  configuration even when the board is not explicitly registered
  ([\#50](https://github.com/rstudio/pins-r/issues/50)).

- Registers “local” board by default, you no longer need to explicitly
  run
  [`board_register_local()`](https://pins.rstudio.com/dev/reference/legacy_local.md)
  ([\#56](https://github.com/rstudio/pins-r/issues/56)).

- Make use of the `rappdirs` package to define the default cache path,
  replaces `~/.pins`. Use
  [`board_cache_path()`](https://pins.rstudio.com/dev/reference/board_cache_path.md)
  to retrieve default cache path.

### Websites

- Fix for data.txt boards created from GitHub boards using large files.

## pins 0.1.2

CRAN release: 2019-09-04

- Support to upload files larger than 50mb in GitHub boards as release
  files.

## pins 0.1.1

CRAN release: 2019-08-30

- Fix CRAN request to explicitly opt-in to use local home path as cache
  by running
  [`board_register_local()`](https://pins.rstudio.com/dev/reference/legacy_local.md).

- Fix error when retrieving pins from Kaggle boards
  ([\#31](https://github.com/rstudio/pins-r/issues/31)).

- Support for large files in GitHub board
  ([\#30](https://github.com/rstudio/pins-r/issues/30)).

## pins 0.1.0

CRAN release: 2019-08-28

- Support for local board.

- Support for Kaggle board.

- Support for packages board.

- Support for RStudio Connect board.

- Support for GitHub board.

- Support for website board.
