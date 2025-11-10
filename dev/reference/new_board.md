# Create a new board

Create a new board

## Usage

``` r
new_board(board, api, cache, ...)

new_board_v0(board, name, cache = NULL, versions = FALSE, ...)

new_board_v1(board, cache, versioned = FALSE, ...)
```

## Arguments

- board:

  The name of the board to register.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- ...:

  Additional parameters required to initialize a particular board.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- versions:

  Should this board be registered with support for versions?

- versioned:

  Should this board be registered with support for versions?
