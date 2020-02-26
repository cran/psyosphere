\name{psyo_rounds}

\alias{psyo_rounds}

\docType{data}

\title{
GPS example of walking in circles
}

\description{
GPS example of walking in circles in \code{\link{psyo}} format.
}

\usage{data(psyo_rounds)}
\format{
  A data frame with 2896 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{\emph{A character or numeric vector.} Id for each unique track.}
    \item{\code{p_id}}{\emph{A character or numeric vector}. Unique by time sorted ID for every coordinate within a track.}
    \item{\code{time}}{\emph{A POSIXct.} Date and time of the coordinate.}
    \item{\code{lon}}{\emph{A numeric vector.} Longitude of a coordinate in degree.}
    \item{\code{lat}}{\emph{A numeric vector.} Latitude of a coordinate in degree.}
    \item{\code{ele}}{\emph{A numeric vector.} Elevation of a coordinate in degree.}
    \item{\code{tracker}}{a numeric vector}
    \item{\code{team}}{a factor with levels \code{1} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{D1} \code{D2} \code{D3}}
    \item{\code{ppn}}{a numeric vector}
  }
}

\seealso{
  \code{\link{val_psyo}}
}

\examples{\donttest{\donttest{
data(psyo_rounds)
plot_tracks(psyo_rounds, t_id = "")
}}}

\keyword{datasets}