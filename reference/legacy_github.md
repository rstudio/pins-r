# GitHub board (legacy API)

**\[deprecated\]**

## Usage

``` r
legacy_github(
  repo,
  branch = NULL,
  token = NULL,
  path = "",
  host = "https://api.github.com",
  name = "github",
  cache = NULL,
  ...
)

board_register_github(
  name = "github",
  repo = NULL,
  branch = NULL,
  token = NULL,
  path = "",
  host = "https://api.github.com",
  cache = NULL,
  ...
)
```

## Arguments

- repo:

  The GitHub repository formatted as 'owner/repo'.

- branch:

  The branch to use to commit pins. Default, `NULL`, will use `main` or
  `master` if present.

- token:

  GitHub personal access token. Uses
  [gitcreds](https://gitcreds.r-lib.org) if not set.

- path:

  The subdirectory in the repo where the pins will be stored.

- host:

  The URL of the GitHub API. You'll need to customise this to use GitHub
  enterprise, e.g. `"https://yourhostname/api/v3"`.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- ...:

  Additional parameters required to initialize a particular board.

## Details

To use a GitHub board, you'll need to set up authentication, following
the instructions at
<https://happygitwithr.com/https-pat.html#https-pat>.

## Large Files

A GitHub repo only supports files under 25MB in size (100MB in theory
but there is additional overhead when using the GitHub API). To store
large files, GitHub
[recommends](https://help.github.com/en/articles/distributing-large-binaries)
storing them using GitHub Releases which support up to 2GB files, which
is what pins uses. You don't need to do anything extra as this will
happen behind the scenes, but don't be surprised if pins creates
releases in your repo.

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires a GitHub API key
board <- legacy_github("owner/repo")
} # }
```
