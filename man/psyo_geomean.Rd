  \name{psyo_geomean}

\alias{psyo_geomean}

\docType{data}

\title{
Example data to demonstrate the geomean function
}

\description{
Data set in the \code{\link{psyo}} format.
}

\usage{data(psyo_geomean)}
\format{
  A data frame with 4 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{\emph{A character vector.} With 1 level: \code{"01.gpx"}}
    \item{\code{p_id}}{\emph{A numeric vector}. Unique by time sorted ID for every coordinate within a track.}
    \item{\code{time}}{\emph{A POSIXct.} Date and time of the coordinate.}
    \item{\code{lon}}{\emph{A numeric vector.} Longitude of a coordinate in degree.}
    \item{\code{lat}}{\emph{A numeric vector.} Latitude of a coordinate in degree.}
    \item{\code{other}}{\emph{A character vector.} With 4 levels \code{"L"}, \code{"e"}, \code{"t"}, and \code{"o"}}
  }
}

\seealso{
  \code{\link{val_psyo}}
}

\examples{\dontrun{

# Simple example ---------------------------------------------------------------

# Get data frame and compress 4 coordinates
data("psyo_geomean")
compressed_tracks <- average_coordinates(psyo_geomean,
                                                             30,
                                                             "seconds")
}}

\keyword{datasets}
