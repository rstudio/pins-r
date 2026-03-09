# Use a Google Drive folder as a board

Pin data to a folder in Google Drive using the googledrive package.

## Usage

``` r
board_gdrive(path, versioned = TRUE, cache = NULL)
```

## Arguments

- path:

  Path to existing directory on Google Drive to store pins. Can be given
  as an actual path like `"path/to/folder"` (character), a file id or
  URL marked with
  [`googledrive::as_id()`](https://googledrive.tidyverse.org/reference/drive_id.html),
  or a
  [googledrive::dribble](https://googledrive.tidyverse.org/reference/dribble.html).

- versioned:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Details

- The functions in pins do not create a new Google Drive folder. You can
  create a new folder from R with
  [`googledrive::drive_mkdir()`](https://googledrive.tidyverse.org/reference/drive_mkdir.html),
  and then set the sharing for your folder with
  [`googledrive::drive_share()`](https://googledrive.tidyverse.org/reference/drive_share.html).

- If you have problems with authentication to Google Drive, learn more
  at
  [`googledrive::drive_auth()`](https://googledrive.tidyverse.org/reference/drive_auth.html).

- `board_gdrive()` is powered by the googledrive package, which is a
  suggested dependency of pins (not required for pins in general). If
  you run into errors when deploying content to a server like
  <https://www.shinyapps.io> or
  [Connect](https://posit.co/products/enterprise/connect/), add
  [`requireNamespace("googledrive")`](https://googledrive.tidyverse.org)
  to your app or document for [automatic dependency
  discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_gdrive("folder-for-my-pins")
board |> pin_write(1:10, "great-integers", type = "json")
board |> pin_read("great-integers")
} # }
```
