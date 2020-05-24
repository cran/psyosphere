\name{export_gpx}

\alias{export_gpx}

\title{
Export tracks as gpx files
}

\description{
Export tracks as gpx files.
}

\usage{
export_gpx(tracks, t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks. The columns \code{track_name} and \code{file_name} have to be defined. }
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\details{

  \code{track_name} is the name of the track.

  \code{file_name} is the file name of the gpx file.

}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  code{\link{export_kml}}
}

\examples{\dontrun{
\dontrun{

# Get tracks

data(psyo)

# Add columns

dir <- tempdir()
psyo[,"track_name"] <- psyo[,"id"]
psyo[,"file_name"] <- file.path(dir, psyo[,"track_name"])

# Export files

export_gpx(tracks = psyo)
}}}
