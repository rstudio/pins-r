# pins 1.3.0

## Breaking changes

* Changed the function signature of `pin_write()` so arguments like `type` and `title` must be passed by name and not position (#792).

## Other improvements

* Removed content and user caches for Connect altogether. Now, we look up 
  usernames and content on the Connect server every time (#793).

* Added new `urls` item to metadata for a pin (#795).

# pins 1.2.2

* Fixed how dots are checked in `pin_write()` to make user-facing messages more 
  clear (#770).

* Improved documentation about Connect caches (#771) and deleting pin versions (#773).

* Added `board_deparse` for `board_url()` (#774).

* Fixed how `board_gdrive()` handles dribble objects (#780, @gorkang and #782).

# pins 1.2.1

* New environment variable `PINS_CACHE_DIR` controls the location of the 
  default cache path (#748).
  
* Added new board for Google Drive `board_gdrive()` (#749).

* Updated test for new arrow release (#764).

# pins 1.2.0

## Breaking changes

* `pin_write()` no longer writes identical pin contents by default, and gains a
  `force_identical_write` argument for writing even when the pin contents are 
  identical to the last version (#735).

## Other improvements

* The `print` method for boards no longer calls `pin_list()` internally (#718).

* `board_s3()` now uses pagination for listing and versioning (#719, @mzorko).

* Added `type = "parquet"` to read and write Parquet files (#729).

* Updated error messages and type checking (#731) along with testing strategy (#724).

* Added new check for whether a new version is the same as the previous version,
  as can happen when writing pin versions very quickly (#727).
  
* Added new `headers` argument for `board_url()`, mostly for authentication, as 
  well as new board for Connect vanity URLs `board_connect_url()` (#732).  

* Fixed bug in `cache_prune()` to correctly find caches for `board_url()` (#734).

# pins 1.1.0

## Breaking changes

* Change the function `board_rsconnect()` to `board_connect()`, following 
  RStudio's rebranding to Posit (#689).

* Changed `type = "csv"` to use R's default value for `stringsAsFactors` i.e. 
  `FALSE` (#664).
  
* Functions for viewing legacy API pins in the RStudio Viewer pane are now 
  deprecated (when possible) or removed (#679).
  
* The functions for accessing Kaggle resource as pins are no longer supported.
  We recommend you use the Kaggle CLI instead (#698).
  
## Other improvements

* Added vignettes describing how to manage custom formats and web-based boards (#631, #685, @ijlyttle).

* Added new board for Google Cloud Storage `board_gcs()` (#695).

* Added new `tags` item to metadata for a pin (#677).

* Improved error message for `pin_versions()` (#657).

* Switched content and user caches for Connect to use environments instead
  of files on disk. This means caches will no longer persist between sessions
  but will be much less likely to end up in a broken state (#667).

* Added `write_board_manifest()` to write a manifest file `_pins.yaml` 
  recording all pins and their versions to the board's root directory.
  This function only works for boards that are not read-only
  (#661, based on work of @ijlyttle).
  
* Updated `board_url()` to handle versions recorded via a manifest file 
  (#681, based on work of @ijlyttle).
  
* Updated code preview on Posit Connect (#690).


# pins 1.0.3

* The `arrow` package is now suggested, rather than imported (#644, @jonthegeek).

* Fixed how Connect usernames are handled in messages, preview, etc (#643).

* Increased datetime precision to the second, for `pin_versions()` and related
  functions (#642, @tomsing1).
  
* Pass the dots from `pin_write()` through to `s3_upload_file()` and 
  `s3_uploade_yaml()` to support S3 tagging, encryption options, etc for 
  pins (#648, #652, @fh-mthomson).  

# pins 1.0.2

* `board_rsconnect()` now correctly finds the created date for pins (#623, 
  @bjfletcher).
  
* `pin_upload()` now better handles path expansion (#585, @sellorm).

* The `pin_reactive_*()` functions now use the hash (rather than the created 
  date) for polling (#595, @thomaszwagerman).
  
# pins 1.0.1

* `board_azure()` now allows you to set a `path` so that multiple boards can
  share the same container (#528, @hongooi73).

* `board_deparse()` is more likely to generate runnable code when used
  with `board_rsconnect()` (#553).

* `legazy_azure()` works once again (#527).

* `legacy_github()` works once again (#549).

* `pin_meta()` now includes pin `name` (#544).

* `board_register()` works better when called directly, due to standardisation 
  of cache paths all computation (#529).

* Drop add-ins since they're not tested or documented (#525)

# pins 1.0.0

pins 1.0.0 includes a new, more explicit, API that includes robust support for versioning. In the modern API, you create a board object which is passed to every `pin_` function instead of "registering" a board that is later refereed to with a string. This leads to code like this:

```R
board <- board_local()
board %>% pin_write(mtcars, "mtcars")
board %>% pin_read("mtcars")
```

The legacy API (`pin()`, `pin_get()`, and `board_register()`) will continue to work, but new features will only be implemented with the new API, so we encourage you to switch to the modern API as quickly as possible. Learn more in `vignette("pins-update")`.

## Modern pin functions

* `pin_read()` and `pin_write()` replace most uses of `pin_get()` and `pin()`.
  `pin_write()` has a `type` argument that allows you to choose how to serialise
  your R objects to disk, allowing you to manage the tradeoffs between speed, 
  generality, and language inter-op, and a `metadata` argument that allows you 
  to store arbitrary metadata (#430).
  
* `pin_download()` and `pin_upload()` are lower-level versions of `pin_read()` 
  and `pin_write()` that work with file paths rather than R objects. They
  replace the use of `pin()` with a path and eliminate the type-instability
  in `pin_get()`, which can return either an R object or a character vector of
  paths.

* `pin_browse()` replaces `board_browse()`, and takes you to a specific pin, 
  either the original source on the internet, or the cached version on your 
  local file system (#435).

* `pin_delete()` replaces `pin_remove()`, and can delete multiple pins (#433).

* `pin_list()` lists all pins in a board.

* `pin_meta()` replaces `pin_info()` and retrieves pin metadata (#418). 

* `pin_search()` replaces `pin_find()`. It is much more limited because the
  previous version was based on assumptions that are not true for many boards.

* `pin_reactive_read()` and `pin_reactive_download()` replace `pin_reactive()`.

* `pin_exists()` reports whether or not a pin exists.

* `pin_version_delete()` allows you to delete a single version.
  `pin_versions_prune()` (#459) allows you to easily prune old versions keeping
  either a specified number of versions, or all versions beneath a certain age.

## Modern boards

This version includes the following modern boards:

* `board_azure()` stores data in Azure's blob storage. It is built on top of 
  [AzureStor](https://github.com/Azure/AzureStor) (#474).

* `board_folder()` is a generalised replacement for the legacy local board.
  `board_folder()` can store data in any directory, making it possible to 
  share boards using shared network drives or on dropbox or similar. If you
  using pins casually and don't want to pick a directory, `board_local()` 
  is a variant of `board_folder()` that stores data in a system data directory.
  
* `board_kaggle_dataset()` and `board_kaggle_competition()` allow you to
  download data from Kaggle. The data is automatically cached so that it's
  only downloaded when it changes.

* `board_ms365()` allow to pin data to MS One Drive and Sharpoint
   (#498, @hongooi73).

* `board_rsconnect()` shares data on 
  [RStudio connect](https://posit.co/products/enterprise/connect/). This board 
  supports both modern and legacy APIs, so that you and your colleagues can use 
  a mixture of pins versions as you transition to pins 1.0.0. Note that the
  compatibility is one directional: you can `pin_read()` pins created by 
  `pin()`, but you can't `pin_get()` pins created by `pin_write()`.
  
* `board_s3()` stores data in Amazon's S3 service. It is built on top of 
  paws.

* `board_url()` lets you create a manual board from a vector of URLs. This is 
  useful because `pin_download()` and `pin_read()` are cached, so they only 
  re-download the data if it has changed since the last time you used it (#409). 
  This board is a replacement for `pin()`'s ability to work directly with URLs

The legacy boards will continue to work with the legacy pins API; we will implement modern versions of the remaining legacy boards based on user feedback.

## Minor improvements and bug fixes

* All board objects now have class beginning with `pins_board_` and also
  inherit from common superclass `pins_board`.

* Pins no longer works with the connections pane. This automatically registered
  code tended to be either dangerous (because it's easy to accidentally leak 
  credentials) or useless (because it relied on variables that the connection 
  pane doesn't capture).

* Pinned data frames are longer converted to tibbles.

* The "packages" board is no longer registered by default; if you want to use
  this you'll need to register with `board_register("packages")`. It has been
  radically simplified so that it will no longer download packages, and it
  `pin_find()` now searches all packages that you have installed, rather than
  a stale snapshot of data in CRAN packages. The CRAN files dataset has
  been removed from the package.

* `board_browse()` now works with local boards.

* `board_rsconnect()` will automatically connect to the current RSC pin board
  when run inside RSC itself (assuming you have version 1.8.8 or later) (#396).

* `cache_browse()`, `cache_info()`, and `cache_prune()` provide some basic
  tooling around the local pins cache maintained by pins (#438).

* `pin_fetch()` has been removed

* `option(pins.invisible)` is now defunct and ignored. 

* You can no longer switch from a versioned pin to an unversioned pin without
  first deleting the pin (#410).

# pins 0.4.5

## Pins

- Support downloading remote files when service returns incompatible
  `data.txt` file (#310).
  
- Support for pins over 100MB in Windows systems (#313).

- Avoid Windows crashing in `pin()` under some locales (#127).

## Boards

- Silenced 'no encoding supplied' warning (#330).

## Local

- `pin_find()` no longer searches text with an undocumented
  regular expression syntax (#270).

## S3

- Default to using HTTPS in S3 boards (#304).

- Support for AWS V4 signatures when registering S3 boards with
  `region` parameter (#304)

## Cloud

- Support for `path` to register a board under a subpath for
  Azure, DigitalOcean, Google Cloud and S3 boards (#200).

- Avoid creating pins named with unsupported characters for
  Azure, DigitalOcean, Google Cloud and S3 boards (#193).

## GitHub

- Properly store relative paths when `path` parameter is
  specified in GitHub boards (#199).
  
- Add support for repos with a 'main' branch as default (#336).

- Add support for large file in private repo releases (#292).

- When a board is registered with `versions = FALSE`, GitHub
  tags are also delete when large files are present (#285).

## RStudio Connect

- Invalid 'account' or 'server' parameters show proper errors (#296).

- Increase total entries retrieved with `pin_find()`, configurable with
  `pins.search.count` (#296).
  
- Fix regression introduced in pins 0.4.2 (#253) preventing users from
  collaborating on existing pins they have access to (#302).
  
- Avoid deleting pin when upload fails to avoid deleting versions (#306).

- Support re-creating pins from pins not previously properly updated (#308).

- Adjust pin preview to only display 1K rows instead of 10K (#315).

- Avoid changing columns names on data frame preview (#190).

- Improve error message when token authentication fails (#327).

# pins 0.4.4

## Pins

- For files bigger than 100MB `pin()` creates symlinks to speed up uploads, can be configured
  using the `pins.link.size` option (#273).
  
- When using `pin(zip = TRUE)` the zip no longer contains local patahs (#277).

## Google Cloud

- Disable caching on `data.txt` to support creating multiple pins at once (#275).

## RStudio

- Prevent connections pane from hanging when multiple pins are updated at once (#280).

## Website

- Support for `pin_get(download = FALSE)` to avoid checking for updates.

## RStudio Connect

- Support for servers with mismatched `http` vs `https` protocols.

- Make use of `RSCONNECT_TAR` when running a report inside RStudio Connect (#293).

# pins 0.4.3

## Boards

- Properly export `board_pin_versions` to allow custom boards extending versions (#265).

## Website

- Fix regression creating pins when using a brand new cloud board (#268).

# pins 0.4.2

## Website

- Fix issue removing pins with custom domain names from cloud boards (#234).

- Fix warning when using `pin()` against storage locations with custom domain name (#237).

- Fix issue where datatxt was not refreshing deleted entries (#239).

## RStudio Connect

- Support for `versions = FALSE` in `board_register()` to avoid using too much space when
  creating pins (#245).
  
- Prevent administrators from overriding pins they don't own, unless the pin is specified 
  as `user/name` (#253).
  
- Support to connect to servers that have a redirect configured when the full server URL
  is not specified in `board_register()` (#256).
  
- Throw error when multiple accounts are associated to the same server (#261).

# pins 0.4.1

## Pin

- When running in production environments (which usually set the `R_CONFIG_ACTIVE`
  environment variable), avoid using shared caches.

- Fix `pin()` failing to update cache when server returns `NULL` etag.

- Support for `custom_metadata` in `pin()` to allow saving custom fields
  in `data.txt` file.
  
- Improve performannce for `pin()` from URLs containing large files that are
  already been cached prerviously by `pin()` (#225).
  
- Avoid showing upload or download progress when creating R Markdown documents
  and other non-interactive use cases (#227).
  
- When pin(url) fails and local cache exists, produce warning and retrieve
  cached version (#231).
  
- Support for `pin(zip = TRUE)` to create a zip file of the given path before
  creating the pin (#232).
  
## RStudio Connect

- Fix when overriding pin with corrupt metadata.

- Avoid using shared caches when running inside RStudio Connect.

- Fixed 'invalid uid' warning when creaating pin undner some Linux servers (#263).

## Kaggle

- Support to find and download competition datasets.

# pins 0.4.0

- Support for versioning in all boards.

- Support for DigitalOcean board.

## Pin

- Finding pins with `pin_find()` sort results by default (#201).

- Avoid `incomplete final line found` warning error wheen reading manifests.

- Support for using `pin()` across multiple concurrent processes (#182).

- Support in `pin_get()` to download arbitrary files from cloud boards
  like Azure, DigitalOcean, GitHub, Google Cloud, RStudio Connect, and S3.

- Fix issue where http HEAD requests could tgimeout and prevent pin from
  downloading in very slow connections.

## RStudio

- Support `access_type` parameter for RStudio Connect.

- `pin()` now refreshes the connections pane.

- `pin_remove()` now refreshes the connections pane.

## RStudio Connect

- Support for `code` parameter in `pin()` to customize R code used in
  the UI to retrieve the pin (#77).

- Improve error message for `pin_get()` with duplicate names (#171).

- Fix board register error when using URL with ports (#195).

- Enable retrieving public pins without authentication (#83).

## GitHub

- Support for `versions = FALSE` in `board_register()` to also delete
  release files when pin is removed (#91).

- Support for `versions = FALSE` in `board_register()` to avoid creating
  versioned GitHub releases (#197).

- Support for committing all github files with a single commit (#197). 

- Support for custom GitHub hosts including GitHub Enterprise using the
  `host` parameter in `board_register_github()` (#163).

## Websites

- Using `pin()` now searches for `data.txt` files in URLs when the URL
  contains no file extension, behavior can be turnned off with
  the `pins.search.datatxt` option.

# pins 0.3.2

## Pins

- `pin_info()` adds support for `metadata` parameter to avoid retrieving pin contents.

## S3

- Added support for `host` parameter to configure "s3.amazonaws.com" to custom locations.

## GitHub

- Fix regression uploading large datasets as release files.

# pins 0.3.1

## Pins

- `pin_info()` prints long character strings in their own line.

- Fixed issue in `pin_remove()` for S3, Azure, GCloud, Google and website boards (#138).

- Fixed issue when pinning complex data frames with nested lists (#142).

## Azure

- Added missing `key` parameter in `board_register_azure()`.

## RStudio

- Fixed connection suggested code when caused by `pin_find()` (#137).

- Fixed connection launcher for Azure connections.

## RStudio Connect

- Fix issue uploading large pins causing `is.character(type) is not TRUE`
  error.

- Fix issue affecting boards registered with trailing slash (#151).

- Improve error messages when a pin fails to be created (#149).

- Added support for `CONNECT_API_KEY` and `CONNECT_SERVER` in place of
  `RSCONNECT_API` and `RSCONNECT_SERVER`, which they are still supported
  for backwards-compatibility.

# pins 0.3.0

- Support for Azure board.

- Support for Google Cloud board.

- Support for S3 board.

## Pins

- `pin()` extracts URLs that end in `.gz` and `zip`, which can be
  disabled with `pin("<url>", extract = FALSE)`.

- `pin()` no longer prints the pin, you can revert this behavior with
  `options(pins.invisible = FALSE)` (#122).

- Show upload progress for files larger than 10mb.

- Avoid changing `data.table` when using `pin()` and `get_pin()`.

- Support for `pin_info()` to describe a pin and `extended = TRUE` in
  `pin_find()` to show all extended information about each pin.

- Support for `extract` parameter in `pin_get()`, `pin()`, and
  `pin_reactive()` to override default file extraction behavior.
  
- Support to extract gzip files when the `R.utils` package is installed.

- Show download progress for files larger than 10mb.

- Support for `I()` to force a pin to be created without additional
  overhead of exporting CSV and other enhancements (#73).

- Support creating automatic `name` when using `pin()` and
  multiple URL.

## Boards

- Avoid "cannot create dir" warning in systems with an
  empty cache folder.
  
## RStudio

- Gracefully handle errors when a pin's preview can't be created.

- Website boards now support the browse menu item in the connection.

- Fix data frames previewing character columns with special
  characters.

- Fix connection code when a board is registered automatically.

## RStudio Connect

- Fix issue where `pin_find()` would not show all available
  pins.

- Fix issue where RStudio Connections pane would not show
  all pins.

- Store all downloaded content under user subfolder insited 
  rsconnect cache.

- Removed `pin_find()` workaround for beta rsconnect server where
  searching was causing timeout.

- Fix for RStudio Connect servers where `/content/` is not 
  used in content URLs.

- Fix issue expanding pins columns in RStudio for pins with
  similar names in RStudio Connect boards.

- Fix issue removing pins with similar names in RStudio
  Connect boards.

## Python

- Support for Python 3.

## Websites

- The `name` parameter in `board_register_datatxt()` is now optional.

- Fix in `pin_find()` to properly search desecription files.

## GitHub

- Various fixes for GitHub boards using the `path` parameter
  when registering the board (#121).

# pins 0.2.0

## RStudio Connect

- Support for retrieving pins shared by others in RStudio
  Connect boards.

- Support for RStudio Connect servers running under a
  subpath in the server.

- Add support for `RSCONNECT_SERVER` environment variable to
  ease configuration of automated RStudio Connect reports.

- Fix intermittent failure to retrieve pins from RStudio
  Connect boards while creating them.

- Fix in RStudio Connect boards to retrieve pins that match
  other pin names (#45).
  
- Fix for data frames with nested data frames in rsconnect
  boards (#36).
  
## GitHub

- `board_register_github()` now checks for the repo to exist (#63).

- Adjusted max upload file to 25mb to avoid "server error" in
  the API, larger files than 25mb uploaded as release files. This
  can be configured using the `pins.github.release` option, which
  deefaults to 25.

- Allow overriding GitHub pin over a pin that partially failed
  to be created.
  
## Boards

- Using a board will attempt to automatically register, such
  that `pin(iris, board = "rsconnect")` would work for
  the default configuration even when the board is not
  explicitly registered (#50).

- Registers "local" board by default, you no longer need to 
  explicitly run `board_register_local()` (#56).

- Make use of the `rappdirs` package to define the default
  cache path, replaces `~/.pins`. Use `board_cache_path()`
  to retrieve default cache path.

## Websites

- Fix for data.txt boards created from GitHub boards using
  large files.

# pins 0.1.2

- Support to upload files larger than 50mb in GitHub boards
  as release files.

# pins 0.1.1

- Fix CRAN request to explicitly opt-in to use local home
  path as cache by running `board_register_local()`.

- Fix error when retrieving pins from Kaggle boards (#31).

- Support for large files in GitHub board (#30).

# pins 0.1.0

- Support for local board.

- Support for Kaggle board.

- Support for packages board.

- Support for RStudio Connect board.

- Support for GitHub board.

- Support for website board.
