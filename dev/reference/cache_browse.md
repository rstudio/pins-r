# Cache management

Most boards maintain a local cache so that if you're reading a pin that
hasn't changed since the last time you read it, it can be rapidly
retrieved from a local cache. These functions help you manage that
cache.

- `cache_browse()`: open the cache directory for interactive
  exploration.

- `cache_info()`: report how much disk space each board's cache uses.

- `cache_prune()`: delete pin versions that you haven't used for `days`
  (you'll be asked to confirm before the deletion happens).

In general, there's no real harm to deleting the cached pins, as they'll
be re-downloaded as needed. The one exception is
[`legacy_local()`](https://pins.rstudio.com/dev/reference/legacy_local.md)
which mistakenly stored its pinned data in the cache directory; do not
touch this directory.

## Usage

``` r
cache_browse()

cache_info()

cache_prune(days = 30)
```

## Arguments

- days:

  Number of days to preserve cached data; any pin versions older than
  `days` will be removed.
