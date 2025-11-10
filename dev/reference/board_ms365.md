# Use a OneDrive or Sharepoint document library as a board

Pin data to a folder in Onedrive or a SharePoint Online document library
using the Microsoft365R package.

## Usage

``` r
board_ms365(
  drive,
  path,
  versioned = TRUE,
  cache = NULL,
  delete_by_item = FALSE
)
```

## Arguments

- drive:

  A OneDrive or SharePoint document library object, of class
  [`Microsoft365R::ms_drive`](https://rdrr.io/pkg/Microsoft365R/man/ms_drive.html).

- path:

  Path to directory to store pins. This can be either a string
  containing the pathname like `"path/to/board"`, or a
  [`Microsoft365R::ms_drive_item`](https://rdrr.io/pkg/Microsoft365R/man/ms_drive_item.html)
  object pointing to the board path.

- versioned:

  Should this board be registered with support for versions?

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- delete_by_item:

  Whether to handle folder deletions on an item-by-item basis, rather
  than deleting the entire folder at once. You may need to set this to
  `TRUE` for a board in SharePoint Online or OneDrive for Business, due
  to document protection policies that prohibit deleting non-empty
  folders.

## Details

Sharing a board in OneDrive (personal or business) is a bit complicated,
as OneDrive normally allows only the person who owns the drive to access
files and folders. First, the drive owner has to set the board folder as
shared with other users, using either the OneDrive web interface or
Microsoft365R's `ms_drive_item$create_share_link()` method. The other
users then call `board_ms365` with a *drive item object* in the `path`
argument, pointing to the shared folder. See the examples below.

Sharing a board in SharePoint Online is much more straightforward,
assuming all users have access to the document library: in this case,
everyone can use the same call `board_ms365(doclib, "path/to/board")`.
If you want to share a board with users outside your team, follow the
same steps for sharing a board in OneDrive.

`board_ms365()` is powered by the Microsoft365R package, which is a
suggested dependency of pins (not required for pins in general). If you
run into errors when deploying content to a server like
<https://www.shinyapps.io> or
[Connect](https://posit.co/products/enterprise/connect/), add
[`requireNamespace("Microsoft365R")`](https://github.com/Azure/Microsoft365R)
to your app or document for [automatic dependency
discovery](https://docs.posit.co/connect/user/troubleshooting/#render-missing-r-package).

## Examples

``` r
if (FALSE) { # \dontrun{
# A board in your personal OneDrive
od <- Microsoft365R::get_personal_onedrive()
board <- board_ms365(od, "myboard")
board |> pin_write(iris)

# A board in OneDrive for Business
odb <- Microsoft365R::get_business_onedrive(tenant = "mytenant")
board <- board_ms365(odb, "myproject/board")

# A board in a SharePoint Online document library
sp <- Microsoft365R::get_sharepoint_site("my site", tenant = "mytenant")
doclib <- sp$get_drive()
board <- board_ms365(doclib, "general/project1/board")


## Sharing a board in OneDrive:
# First, create the board on the drive owner's side
board <- board_ms365(od, "myboard")

# Next, let other users write to the folder
# - set the expiry to NULL if you want the folder to be permanently available
od$get_item("myboard")$create_share_link("edit", expiry="30 days")

# On the recipient's side: find the shared folder item, then pass it to board_ms365
shared_items <- od$list_shared_items()
board_folder <- shared_items$remoteItem[[which(shared_items$name == "myboard")]]
board <- board_ms365(od, board_folder)
} # }
```
