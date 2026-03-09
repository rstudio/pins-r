# Retrieve default cache path

Retrieves the default path used to cache boards and pins. Makes use of
[`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)
for cache folders defined by each OS. Remember that you can set the
cache location for an individual board object via the `cache` argument.

## Usage

``` r
board_cache_path(name)
```

## Arguments

- name:

  Board name

## Details

There are several environment variables available to control the
location of the default pins cache:

- Use `PINS_CACHE_DIR` to set the cache path for *only* pins functions

- Use `R_USER_CACHE_DIR` to set the cache path for all functions that
  use rappdirs

On system like AWS Lambda that is read only (for example, only `/tmp` is
writeable), set either of these to
[`base::tempdir()`](https://rdrr.io/r/base/tempfile.html). You may also
need to set environment variables like `HOME` and/or `R_USER_DATA_DIR`
to the session temporary directory.

## Examples

``` r
# retrieve default cache path
board_cache_path("local")
#> ~/.cache/pins/local

# set with env vars:
withr::with_envvar(
  c("PINS_CACHE_DIR" = "/path/to/cache"),
  board_cache_path("local")
)
#> /path/to/cache/local
withr::with_envvar(
  c("R_USER_CACHE_DIR" = "/path/to/cache"),
  board_cache_path("local")
)
#> /path/to/cache/pins/local
```
