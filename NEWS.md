# pins 0.1.2.9000 (unreleased)

- Make use of the `rappdirs` package to define the default
  cache path, replaces `~/.pins`. Use `board_cache_path()`
  to retrieve default cache path.

- Fix for data.txt boards created from GitHub boards using
  large files.

- Fix for data frames with nested data frames in rsconnect
  boards (#36).

# pins 0.1.2

- Support to upload files larger than 50mb in GitHub boards
  as release files.

# pins 0.1.1

- Fix CRAN request to explicitly opt-in to use local home
  path as cache by running `board_register_local()`.

- Fix error when retrieving pins from Kaggle boards (#31).

- Support for large files in GitHub board (#30).

# pins 0.1.0

- Support for local board.

- Support for Kaggle board.

- Support for packages board.

- Support for RStudio Connect board.

- Support for GitHub board.

- Support for website board.
