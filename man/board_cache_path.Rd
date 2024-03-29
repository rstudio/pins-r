% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board.R
\name{board_cache_path}
\alias{board_cache_path}
\title{Retrieve default cache path}
\usage{
board_cache_path(name)
}
\arguments{
\item{name}{Board name}
}
\description{
Retrieves the default path used to cache boards and pins. Makes
use of \code{\link[rappdirs:user_cache_dir]{rappdirs::user_cache_dir()}} for cache folders defined by each OS.
Remember that you can set the cache location for an individual board object
via the \code{cache} argument.
}
\details{
There are several environment variables available to control the location of
the default pins cache:
\itemize{
\item Use \code{PINS_CACHE_DIR} to set the cache path for \emph{only} pins functions
\item Use \code{R_USER_CACHE_DIR} to set the cache path for all functions that use rappdirs
}

On system like AWS Lambda that is read only (for example, only \verb{/tmp} is
writeable), set either of these to \code{\link[base:tempfile]{base::tempdir()}}. You may also need to
set environment variables like \code{HOME} and/or \code{R_USER_DATA_DIR} to the
session temporary directory.
}
\examples{
# retrieve default cache path
board_cache_path("local")

# set with env vars:
withr::with_envvar(
  c("PINS_CACHE_DIR" = "/path/to/cache"),
  board_cache_path("local")
)
withr::with_envvar(
  c("R_USER_CACHE_DIR" = "/path/to/cache"),
  board_cache_path("local")
)

}
