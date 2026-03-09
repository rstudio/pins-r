# List, delete, and prune pin versions

- `pin_versions()` lists available versions a pin.

- `pin_versions_prune()` deletes old versions.

- `pin_version_delete()` deletes a single version.

## Usage

``` r
pin_versions(board, name, ...)

pin_version_delete(board, name, version, ...)

pin_versions_prune(board, name, n = NULL, days = NULL, ...)
```

## Arguments

- board, name:

  A pair of board and pin name. For modern boards, use
  `board |> pin_versions(name)`. For backward compatibility with the
  legacy API, you can also use `pin_versions(name)` or
  `pin_version(name, board)`.

- ...:

  Additional arguments passed on to methods for a specific board.

- version:

  Version identifier.

- n, days:

  Pick one of `n` or `days` to choose how many versions to keep. `n = 3`
  will keep the last three versions, `days = 14` will keep all the
  versions created in the 14 days. Regardless of what values you set,
  `pin_versions_prune()` will never delete the most recent version.

## Value

A data frame with at least a `version` column. Some boards may provided
additional data.

## Examples

``` r
board <- board_temp(versioned = TRUE)

board |> pin_write(data.frame(x = 1:5), name = "df")
#> Guessing `type = 'rds'`
#> Creating new version '20260309T143714Z-dc8e8'
#> Writing to pin 'df'
board |> pin_write(data.frame(x = 2:6), name = "df")
#> Guessing `type = 'rds'`
#> Creating new version '20260309T143715Z-3e72e'
#> Writing to pin 'df'
board |> pin_write(data.frame(x = 3:7), name = "df")
#> Guessing `type = 'rds'`
#> Creating new version '20260309T143715Z-62504'
#> Writing to pin 'df'

# pin_read() returns the latest version by default
board |> pin_read("df")
#>   x
#> 1 3
#> 2 4
#> 3 5
#> 4 6
#> 5 7

# but you can return earlier versions if needed
board |> pin_versions("df")
#> # A tibble: 3 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260309T143714Z-dc8e8 2026-03-09 14:37:14 dc8e8
#> 2 20260309T143715Z-3e72e 2026-03-09 14:37:15 3e72e
#> 3 20260309T143715Z-62504 2026-03-09 14:37:15 62504

ver <- pin_versions(board, "df")$version[[1]]
board |> pin_read("df", version = ver)
#>   x
#> 1 1
#> 2 2
#> 3 3
#> 4 4
#> 5 5

# delete all versions created more than 30 days ago
board |> pin_versions_prune("df", days = 30)
#> No old versions to delete
```
