\name{export_kml}

\alias{export_kml}

\title{
Export tracks as kml files
}

\description{
Export tracks as kml files.
}

\usage{
export_kml(tracks, t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks. The columns \code{track_name}, \code{file_name}, \code{track_color} have to be defined. }
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\details{

  \code{track_name} is the name of the track.

  \code{file_name} is the file name of the kml file.

  \code{color} is the color of the track. For instance \code{"ffaa00bb"} sets the transparency to \code{bb} in exdecimal or 73\%, sets blue to \code{00}, sets green to \code{aa}, and sets red to \code{ff}. See also \code{\link[ggplot2]{aes_colour_fill_alpha}}.

}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  code{\link{export_gpx}}
}

\examples{\donttest{
\dontrun{

# Get tracks

data(psyo)

# Add columns

dir <- tempdir()
psyo[,"track_color"] <- "bb00aaff"
psyo[,"track_name"] <- psyo[,"id"]
psyo[,"file_name"] <- file.path(dir, paste0(psyo[,"track_name"], ".kml"))

# Export files

export_kml(psyo)
}
}}
