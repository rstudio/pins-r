# Kaggle board (legacy API)

**\[deprecated\]**

## Usage

``` r
legacy_kaggle(token = NULL, name = "kaggle", ...)

board_register_kaggle(name = "kaggle", token = NULL, cache = NULL, ...)
```

## Arguments

- token:

  The Kaggle token as a path to the `kaggle.json` file, can be `NULL` if
  the `~/.kaggle/kaggle.json` file already exists.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

## Details

These functions are no longer supported because of changes to the Kaggle
API and will be removed in a future version of pins. We recommend that
you use the [Kaggle CLI](https://www.kaggle.com/docs/api) instead.

To use a Kaggle board, you need to first download a token file from your
account.

## Sharing

When working in teams, you might want to share your pins with others.
For You can do by adding users or making the dataset public on Kaggle's
website.

Once you share with specific users, they can follow the same steps to
register a Kaggle board which allows them to download and upload pins

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires a Kaggle API token
board <- legacy_kaggle(token = "path/to/kaggle.json")

pin_find("crowdflower", board = board)

# names starting with c/ are competitions
pin_get("c/crowdflower-weather-twitter", board = board)
} # }
```
