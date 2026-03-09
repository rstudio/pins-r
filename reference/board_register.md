# Board registry (legacy API)

**\[deprecated\]**

The legacy pins API uses a board registry, where you first register a
board then refer to it by name in calls to pin functions.

## Usage

``` r
board_register(board, name = NULL, cache = NULL, versions = NULL, ...)

board_register_rsconnect(
  name = "rsconnect",
  server = NULL,
  account = NULL,
  key = NULL,
  output_files = FALSE,
  cache = NULL,
  ...
)

board_deregister(name, ...)

board_default()

board_list()

board_get(name)
```
