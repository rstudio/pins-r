# Azure board (legacy API)

**\[deprecated\]**

To use Microsoft Azure Storage as a board, you'll need an Azure Storage
account, an Azure Storage container, and an Azure Storage key.

## Usage

``` r
legacy_azure(
  container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
  account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
  key = Sys.getenv("AZURE_STORAGE_KEY"),
  cache = NULL,
  name = "azure",
  ...
)

board_register_azure(
  name = "azure",
  container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
  account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
  key = Sys.getenv("AZURE_STORAGE_KEY"),
  cache = NULL,
  path = NULL,
  ...
)
```

## Arguments

- container:

  The name of the Azure Storage container.

- account:

  The name of the Azure Storage account.

- key:

  The access key for the Azure Storage container. You can find this
  under "Access keys" in your storage account settings.

  The `key` is equivalent to a password, so generally should not be
  stored in your script. The easiest alternative is to store it in the
  `AZURE_STORAGE_KEY` environment variable, which `legacy_azure()` will
  use by default.

- cache:

  Cache path. Every board requires a local cache to avoid downloading
  files multiple times. The default stores in a standard cache location
  for your operating system, but you can override if needed.

- name:

  An optional name used identify the board. This is no longer generally
  needed since you should be passing around an explicit board object.

- ...:

  Additional parameters required to initialize a particular board.

- path:

  Subdirectory within `url`

## Examples

``` r
if (FALSE) { # \dontrun{
# the following example requires an Azure Storage key
board_register_azure(
  container = "pinscontainer",
  account = "pinsstorage",
  key = "abcabcabcabcabcabcabcabcabcab=="
)
} # }
```
