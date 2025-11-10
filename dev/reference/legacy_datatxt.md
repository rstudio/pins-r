# Remote "data.txt" board (legacy API)

**\[deprecated\]**

## Usage

``` r
legacy_datatxt(
  url,
  headers = NULL,
  cache = NULL,
  needs_index = TRUE,
  browse_url = url,
  index_updated = NULL,
  index_randomize = FALSE,
  path = NULL,
  versions = FALSE,
  name = NULL,
  ...
)

board_register_datatxt(url, name = NULL, headers = NULL, cache = NULL, ...)
```

## Arguments

- url:

  Path to the `data.txt` file or directory containing it.

- headers:

  Optional list of headers to include or a function to generate them.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- needs_index:

  Does this board have an index file?

- browse_url:

  Not currently used

- index_updated:

  Callback function used to update index

- index_randomize:

  When retrieving `data.txt` at a parameter with random query string to
  defeat caching?

- path:

  Subdirectory within `url`

- versions:

  Should this board be registered with support for versions?

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

## Details

Use board that for a website that uses the
[data.txt](https://datatxt.org) specification. A `data.txt` file is a
YAML that provides some basic metadata about a directory of files.
