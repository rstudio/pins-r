% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board.R
\name{write_board_manifest}
\alias{write_board_manifest}
\title{Write board manifest file to board's root directory}
\usage{
write_board_manifest(board, ...)
}
\arguments{
\item{board}{A pin board that is \emph{not} read-only.}

\item{...}{Additional arguments passed on to methods for a specific board.}
}
\value{
The board, invisibly
}
\description{
A board manifest file records all the pins, along with their
versions, stored on a board.
This can be useful for a board built using, for example,
\code{\link[=board_folder]{board_folder()}} or \code{\link[=board_s3]{board_s3()}}, then served as a website,
such that others can consume using \code{\link[=board_url]{board_url()}}.
The manifest file is \emph{not} versioned like a pin is, and this function
will overwrite any existing \verb{_pins.yaml} file on your board. It is
your responsibility as the user to keep the manifest up to date.

Some examples are provided in \code{vignette("using-board-url")}.
}
\details{
This function is not supported for read-only boards.
It is called for the side-effect of writing a manifest file,
\verb{_pins.yaml}, to the root directory of the  \code{board}. (This will
not work in the unlikely event that you attempt to create a pin
called \code{"_pins.yaml"}.)

The behavior of the legacy API (for example, \code{\link[=pin_find]{pin_find()}}) is unspecified
once you have written a board manifest file to a board's root directory.
We recommend you only use \code{write_board_manifest()} with modern boards.
}
\examples{
board <- board_temp()
pin_write(board, mtcars, "mtcars-csv", type = "csv")
pin_write(board, mtcars, "mtcars-json", type = "json")

write_board_manifest(board)

# see the manifest's format:
fs::path(board$path, "_pins.yaml") \%>\% readLines() \%>\% cat(sep = "\n")

# if you write another pin, the manifest file is out of date:
pin_write(board, 1:10, "nice-numbers", type = "json")

# you decide when to update the manifest:
write_board_manifest(board)

}
