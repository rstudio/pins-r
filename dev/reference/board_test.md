# Test Board

**\[deprecated\]**

## Usage

``` r
board_test(board, exclude = list(), suite = c("default", "versions"))
```

## Arguments

- board:

  The name of the board to test.

- exclude:

  Names of tests to exclude form test.

- suite:

  The test suite to run, currently only `"versions"` or `default` are
  supported.

## Details

Tests a particular board, useful only when creating new boards.
