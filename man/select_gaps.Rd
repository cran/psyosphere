\name{select_gaps}

\alias{select_gaps}

\title{
Select all coordinates with a gap
}
\description{
Select all coordinates with a gap
}
\usage{
select_gaps(tracks, cgaps)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{mark_time_gaps}}, \code{\link{mark_speed_gaps}}, \code{\link{select_without_gaps}}
}

\examples{\donttest{
# Get data
data(psyo_rounds2)
tracks <- psyo_rounds2

# Calculations
tracks <- average_duplicates(tracks)
tracks <- t_time_difference(tracks)

tracks <- mark_time_gaps(tracks)
tracks <- select_gaps(tracks,"time_gap")
}}
