#' Retrieve Default Cache Path
#'
#' Retrieves the default path used to cache boards and pins. Makes
#' use of the \code{rappdirs} package to use cache folders
#' defined by each OS.
#'
#' @examples
#' # retrieve default cache path
#' board_cache_path()
#' @export
board_cache_path <- function() {
  # if a configuration is present this could mean we are running in a production environment without user caches
  if (nchar(Sys.getenv("R_CONFIG_ACTIVE")) > 0 && nchar(Sys.getenv("PINS_USE_CACHE")) == 0)
    tempfile()
  else
    getOption("pins.path", rappdirs::user_cache_dir("pins"))
}

#' Register Local Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a local folder as a board.
#'
#' @param name Optional name for this board, defaults to 'local'.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @seealso board_register
#'
#' @examples
#' # register local board using a temp folder
#' board_register_local(cache = tempfile())
#' @export
board_register_local <- function(name = "local",
                                 cache = board_cache_path(),
                                 ...) {
  board_register("local", name = name,
                          cache = cache,
                          ...)
}

#' Register GitHub Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a GitHub repo as a board.
#'
#' @param name Optional name for this board, defaults to 'github'.
#' @param repo The GitHub repository formatted as 'owner/repo', can be
#'   \code{NULL} if the \code{GITHUB_PAT} environment variable is set.
#' @param branch The branch to use when commiting pins.
#' @param token Token to use when \code{GITHUB_PAT} is not specified.
#' @param path The subdirectory in the repo where the pins will be stored.
#' @param host The URL hosting the GitHub API, defaults to \code{"https://api.github.com"}.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires a GitHub repo to be manually created; otherwise,
#' registering a GitHub board will fail.
#'
#' When a file upload exceeds 25MB, a GitHub release file will be used since
#' they support up to 2GB file uploads. This threshold can be configured through
#' the \code{pins.github.release} option which is specified in megabytes and
#' defaults to \code{25}.
#'
#' When using GitHub Enterprise, consider customizing the \code{host} parameter to
#' \code{"https://yourhostname/api/v3"}.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a GitHub API key
#' board_register_github(repo = "owner/repo")
#' }
#' @export
board_register_github <- function(name = "github",
                                  repo = NULL,
                                  branch = "master",
                                  token = NULL,
                                  path = "",
                                  host = "https://api.github.com",
                                  cache = board_cache_path(),
                                  ...) {
  board_register("github", name = name,
                           repo = repo,
                           branch = branch,
                           token = token,
                           path = path,
                           cache = cache,
                           host = host,
                           ...)
}

#' Register Kaggle Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register Kaggle as a board.
#'
#' @param name Optional name for this board, defaults to 'kaggle'.
#' @param token The Kaggle token as a path to the \code{kaggle.json} file, can
#'   be \code{NULL} if the \code{~/.kaggle/kaggle.json} file already exists.
#' @param overwrite Should \code{~/.kaggle/kaggle.json} be overriden?
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a Kaggle API token
#' board_register_kaggle(token = "path/to/kaggle.json")
#' }
#'
#' @export
board_register_kaggle <- function(name = "kaggle",
                                  token = NULL,
                                  overwrite = FALSE,
                                  cache = board_cache_path(),
                                  ...) {
  board_register("kaggle", name = name,
                           token = token,
                           overwrite = overwrite,
                           cache = cache,
                           ...)
}

#' Register RStudio Connect Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register RStudio Connecet as a board.
#'
#' @param name Optional name for this board, defaults to 'rsconnect'.
#' @param server Optional address to RStudio Connect server.
#' @param account Optional account name to use with RStudio Connect.
#' @param key The RStudio Connect API key.
#' @param output_files Should the output in an automated report create output files?
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following examples require an RStudio Connect API key
#'
#' # register from rstudio
#' board_register_rsconnect()
#'
#' # register from rstudio with multiple servers
#' board_register_rsconnect(server = "https://rstudio-connect-server")
#'
#' # register from rstudio with multiple account
#' board_register_rsconnect(account = "account-name")
#'
#' # register automated report for rstudio connect
#' board_register_rsconnect(key = Sys.getenv("CONNECT_API_KEY"),
#'                          server = Sys.getenv("CONNECT_SERVER"))
#' }
#'
#' @export
board_register_rsconnect <- function(name = "rsconnect",
                                     server = NULL,
                                     account = NULL,
                                     key = NULL,
                                     output_files = FALSE,
                                     cache = board_cache_path(),
                                     ...) {
  board_register("rsconnect", name = name,
                              server = server,
                              account = account,
                              key = key,
                              output_files = output_files,
                              cache = cache,
                              ...)
}

#' Register Data TXT Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register as a board a website describing resources with a \code{data.txt} file.
#'
#' @param url Path to the \code{data.txt} file or path containing it.
#' @param name The name for this board, usually the domain name of the website.
#' @param headers Optional list of headers to include or a function to generate them.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @seealso board_register
#'
#' @examples
#'
#' # register website board using datatxt file
#' board_register_datatxt(url = "https://datatxt.org/data.txt",
#'                        name = "txtexample",
#'                        cache = tempfile())
#'
#' # find pins
#' pin_find(board = "txtexample")
#'
#' @export
board_register_datatxt <- function(url,
                                   name = NULL,
                                   headers = NULL,
                                   cache = board_cache_path(),
                                   ...) {
  board_register("datatxt", name = name,
                            url = url,
                            headers = headers,
                            cache = cache,
                            ...)
}

#' Register S3 Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register an Amazon S3 bucket as a board.
#'
#' @param name Optional name for this board, defaults to 's3'.
#' @param bucket The name of the Amazon S3 bucket. Defaults to the \code{AWS_BUCKET} environment
#'   variable.
#' @param key The key of the Amazon S3 bucket. Defaults to the \code{AWS_ACCESS_KEY_ID} environment
#'   variable.
#' @param secret The secret of the Amazon S3 bucket. Defaults to the \code{AWS_SECRET_ACCESS_KEY} environment
#'   variable.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param host The host to use for storage, defaults to \code{"s3.amazonaws.com"}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires an Amazon S3 bucket to be manually created; otherwise,
#' registering an S3 board will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires an Amazon S3 API key
#' board_register_s3(bucket = "s3bucket")
#' }
#' @export
board_register_s3 <- function(name = "s3",
                              bucket = Sys.getenv("AWS_BUCKET"),
                              key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                              secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                              cache = board_cache_path(),
                              host = "s3.amazonaws.com",
                              ...) {
  board_register("s3",
                 name = name,
                 bucket = bucket,
                 key = key,
                 secret = secret,
                 cache = cache,
                 ...)
}

#' Register Azure Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a Microsoft Azure Storage Blob as a board.
#'
#' @param name Optional name for this board, defaults to 'azure'.
#' @param container The name of the Azure Storage container. Defaults to the \code{AZURE_STORAGE_CONTAINER} environment
#'   variable.
#' @param account The account of the Azure Storage container. Defaults to the \code{AZURE_STORAGE_ACCOUNT} environment
#'   variable.
#' @param key The key of the Azure Storage container Defaults to the \code{AZURE_STORAGE_KEY} environment
#'   variable.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires an Azure Storage container to be manually created; otherwise,
#' registering an Azire board will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires an Azure Storage key
#' board_register_azure(container = "pinscontainer",
#'                      account = "pinsstorage",
#'                      key = "abcabcabcabcabcabcabcabcabcab==")
#' }
#' @export
board_register_azure <- function(name = "azure",
                                 container = Sys.getenv("AZURE_STORAGE_CONTAINER"),
                                 account = Sys.getenv("AZURE_STORAGE_ACCOUNT"),
                                 key = Sys.getenv("AZURE_STORAGE_KEY"),
                                 cache = board_cache_path(),
                                 ...) {
  board_register("azure",
                 name = name,
                 account = account,
                 container = container,
                 key = key,
                 cache = cache,
                 ...)
}

#' Register Google Cloud Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a Google Cloud Storage container as a board.
#'
#' @param name Optional name for this board, defaults to 'gcloud'.
#' @param bucket The name of the Google Cloud Storage bucket. Defaults to the \code{GCLOUD_STORAGE_BUCKET} environment
#'   variable.
#' @param token The access token of the Google Cloud Storage container. Defaults to use the Google Cloud SDK if configured.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires a Google Cloud Storage container to be manually created; otherwise,
#' registering a Google Cloud board will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires the Google Cloud SDK to be configured
#' board_register_gcloud(container = "gcloudcontainer")
#' }
#' @export
board_register_gcloud <- function(name = "gcloud",
                                  bucket = Sys.getenv("GCLOUD_STORAGE_BUCKET"),
                                  token = NULL,
                                  cache = board_cache_path(),
                                  ...) {
  board_register("gcloud",
                 name = name,
                 bucket = bucket,
                 token = token,
                 cache = cache,
                 ...)
}

#' Register DigitalOcean Board
#'
#' Wrapper with explicit parameters over \code{board_register()} to
#' register a DigitalOcean Spaces board.
#'
#' @param name Optional name for this board, defaults to 's3'.
#' @param space The name of the DigitalOcean space. Defaults to the \code{DO_SPACE} environment
#'   variable.
#' @param key The key of the DigitalOcean space. Defaults to the \code{DO_ACCESS_KEY_ID} environment
#'   variable.
#' @param secret The secret of the DigitalOcean space. Defaults to the \code{DO_SECRET_ACCESS_KEY} environment
#'   variable.
#' @param datacenter The datacenter of the DigitalOcean space. Defaults to the \code{DO_DATACENTER} environment
#'   variable.
#' @param cache The local folder to use as a cache, defaults to \code{board_cache_path()}.
#' @param host The host to use for storage, defaults to \code{"digitaloceanspaces.com"}.
#' @param ... Additional parameters required to initialize a particular board.
#'
#' @details
#'
#' This function requires a DigitalOcean space to be manually created; otherwise,
#' registering a DigitalOcean space will fail.
#'
#' @seealso board_register
#'
#' @examples
#' \dontrun{
#' # the following example requires a DigitalOcean Spaces API key
#' board_register_s3(bucket = "s3bucket")
#' }
#' @export
board_register_dospace <- function(name = "dospace",
                                   space = Sys.getenv("DO_SPACE"),
                                   key = Sys.getenv("DO_ACCESS_KEY_ID"),
                                   secret = Sys.getenv("DO_SECRET_ACCESS_KEY"),
                                   datacenter = Sys.getenv("DO_DATACENTER"),
                                   cache = board_cache_path(),
                                   host = "digitaloceanspaces.com",
                                   ...) {
  board_register("dospace",
                 name = name,
                 space = space,
                 key = key,
                 secret = secret,
                 datacenter = datacenter,
                 cache = cache,
                 host = host,
                 ...)
}
