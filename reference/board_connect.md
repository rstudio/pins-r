# Use Posit Connect as board

To use a Posit Connect board, you need to first authenticate. The
easiest way to do so is using the RStudio IDE and choosing **Tools** -
**Global Options** - **Publishing** - **Connect**, then following the
instructions.

You can share pins with others in Posit Connect by changing the viewers
of the document to specific users or groups. This is accomplished by
opening the new published pin and then changing access under the
settings tab. After you've shared the pin, it will be automatically
available to others.

## Usage

``` r
board_connect(
  auth = c("auto", "manual", "envvar", "rsconnect"),
  server = NULL,
  account = NULL,
  key = NULL,
  cache = NULL,
  name = "posit-connect",
  versioned = TRUE,
  use_cache_on_failure = is_interactive()
)

board_rsconnect(
  auth = c("auto", "manual", "envvar", "rsconnect"),
  server = NULL,
  account = NULL,
  key = NULL,
  output_files = FALSE,
  cache = NULL,
  name = "posit-connect",
  versioned = TRUE,
  use_cache_on_failure = is_interactive()
)
```

## Arguments

- auth:

  There are three ways to authenticate:

  - `auth = "manual"` uses arguments `server` and `key`.

  - `auth = "envvar"` uses environment variables `CONNECT_API_KEY` and
    `CONNECT_SERVER`.

  - `auth = "rsconnect"` uses servers registered with the rsconnect
    package (filtered by `server` and `account`, if provided)

  The default, `auth = "auto"`, automatically picks between the three
  options, using `"manual"` if `server` and `key` are provided,
  `"envvar"` if both environment variables are set, and `"rsconnect"`
  otherwise.

- server:

  For `auth = "manual"` or `auth = 'envvar'`, the full url to the
  server, like `http://server.posit.co/rsc` or
  `https://connect.posit.co/`. For `auth = 'rsconnect'` a host name used
  to disambiguate Connect accounts, like `server.posit.co` or
  `connect.posit.co`.

- account:

  A user name used to disambiguate multiple Connect accounts.

- key:

  The Posit Connect API key.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- versioned:

  Should this board be registered with support for versions?

- use_cache_on_failure:

  If the pin fails to download, is it OK to use the last cached version?
  Defaults to `is_interactive()` so you'll be robust to poor internet
  connectivity when exploring interactively, but you'll get clear errors
  when the code is deployed. Note that this argument controls whether
  you use the cache for reading pins, but you can't create a board
  object unless you can connect to your Connect server.

- output_files:

  **\[deprecated\]** No longer supported.

## Public pins

If your Posit Connect instance allows it, you can share a pin publicly
by setting the access type to `all`:

    board |> pin_write(my_df, access_type = "all")

(You can also do this in Posit Connect by setting "Access" to "Anyone -
no login required")

Now anyone can read your pin through
[`board_url()`](https://pins.rstudio.com/reference/board_url.md):

    board <- board_url(c(
      numbers = "https://pub.current.posit.team/public/great-numbers/"
    ))
    board |> pin_read("numbers")

You can find the URL of a pin with
[`pin_browse()`](https://pins.rstudio.com/reference/pin_browse.md).

## HTML preview

When you write a pin to Posit Connect, a landing page is created and
published together with the pin file. This HTML landing page shows a
table preview for rectangular data, but you can opt out of the table
preview:

    board |> pin_write(my_df, preview_data = FALSE)

## See also

Other boards:
[`board_connect_url()`](https://pins.rstudio.com/reference/board_connect_url.md),
[`board_folder()`](https://pins.rstudio.com/reference/board_folder.md),
[`board_url()`](https://pins.rstudio.com/reference/board_url.md)

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_connect()
# Share the mtcars with your team
board |> pin_write(mtcars, "mtcars")

# Download a shared dataset
board |> pin_read("timothy/mtcars")
} # }
```
