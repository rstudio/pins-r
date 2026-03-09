# Use kaggle datasets/competitions as a board

These functions are no longer supported because of changes to the Kaggle
API and will be removed in a future version of pins. We recommend that
you use the [Kaggle CLI](https://www.kaggle.com/docs/api) instead.

`board_kaggle_competition()` allows you to treat a Kaggle competition
like a read-only board, making it easy get the data on to your computer.
`board_kaggle_dataset()` lets you upload and download files to and from
a kaggle dataset. Data is only re-downloaded when it changes.

These boards work best with
[`pin_download()`](https://pins.rstudio.com/reference/pin_download.md)
and [`pin_upload()`](https://pins.rstudio.com/reference/pin_download.md)
since [`pin_read()`](https://pins.rstudio.com/reference/pin_read.md) and
[`pin_write()`](https://pins.rstudio.com/reference/pin_read.md) are not
a good fit to the Kaggle model.

## Usage

``` r
board_kaggle_competitions(username = NULL, key = NULL, cache = NULL)

# S3 method for class 'pins_board_kaggle_competition'
pin_search(
  board,
  search = NULL,
  sort_by = c("grouped", "prize", "earliestDeadline", "latestDeadline", "numberOfTeams",
    "recentlyCreated"),
  page = 1,
  user = NULL,
  ...
)

board_kaggle_dataset(username = NULL, key = NULL, cache = NULL)

# S3 method for class 'pins_board_kaggle_dataset'
pin_search(
  board,
  search = NULL,
  sort_by = c("hottest", "votes", "updated", "active"),
  page = 1,
  user = NULL,
  ...
)

# S3 method for class 'pins_board_kaggle_dataset'
pin_store(
  board,
  name,
  paths,
  metadata,
  versioned = NULL,
  ...,
  private = TRUE,
  license = "CC0-1.0"
)
```

## Arguments

- username, key:

  Typically you'll authenticate using the `"~/.kaggle/kaggle.json"` file
  downloaded from your account page (by clicking "Create New API Token".
  However, if necessary you can supply the `username` and `key`
  arguments here; this can be useful for testing.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- board:

  The name of the board to register.

- search:

  A string to search for in pin name and title. Use `NULL` to return all
  pins.

- sort_by:

  How to sort the results.

- page:

  Which page of results to retrieve.

- user:

  If non-`NULL` filter to specified user.

- ...:

  Additional parameters required to initialize a particular board.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- paths:

  A character vector of file paths to upload to `board`.

- metadata:

  A list containing additional metadata to store with the pin. When
  retrieving the pin, this will be stored in the `user` key, to avoid
  potential clashes with the metadata that pins itself uses.

- versioned:

  Should this board be registered with support for versions?

- private:

  Should the dataset be private (`TRUE`, the default) or public
  (`FALSE`)?

- license:

  How should the data be licensed?

## Examples

``` r
if (FALSE) { # \dontrun{
board <- board_kaggle_competitions()
board

board |> pin_meta("titanic")
paths <- board |> pin_download("titanic")
paths
head(read.csv(paths[[1]]))
head(read.csv(paths[[2]]))
} # }
if (FALSE) { # \dontrun{
board <- board_kaggle_dataset()

board |> pin_search("cats")
board |> pin_exists("rturley/pet-breed-characteristics")
board |> pin_meta("rturley/pet-breed-characteristics")
board |> pin_versions("rturley/pet-breed-characteristics")

board |> pin_versions("imsparsh/animal-breed-cats-and-dogs")
} # }
```
