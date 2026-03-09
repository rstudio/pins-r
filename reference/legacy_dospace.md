# DigitalOcean board (legacy API)

**\[deprecated\]**

## Usage

``` r
legacy_dospace(
  space = Sys.getenv("DO_SPACE"),
  key = Sys.getenv("DO_ACCESS_KEY_ID"),
  secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
  datacenter = Sys.getenv("DO_DATACENTER"),
  cache = NULL,
  host = "digitaloceanspaces.com",
  name = "dospace",
  ...
)

board_register_dospace(
  name = "dospace",
  space = Sys.getenv("DO_SPACE"),
  key = Sys.getenv("DO_ACCESS_KEY_ID"),
  secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
  datacenter = Sys.getenv("DO_DATACENTER"),
  cache = NULL,
  host = "digitaloceanspaces.com",
  path = NULL,
  ...
)
```

## Arguments

- space:

  The name of the DigitalOcean space.

- key, secret:

  The key and secret for your space. You can create a key and secret in
  the "Spaces access keys" in your API settings.

  The `secret` is equivalent to a password, so generally should not be
  stored in your script. The easiest alternative is to store it in the
  `DO_SECRET_ACCESS_KEY` environment variable, which `legacy_dospace()`
  will use by default.

- datacenter:

  The datacenter name.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- host:

  The host to use for storage, defaults to `"digitaloceanspaces.com"`.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

- path:

  Subdirectory within `url`

## Details

To use DigitalOcean Spaces as a board, you first need an DigitalOcean
space and a storage key. You can sign-up and create those at
[digitalocean.com](https://www.digitalocean.com/).

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires a DigitalOcean Spaces API key
board <- legacy_dospace(bucket = "s3bucket")
} # }
```
