\name{t_bearing}

\alias{t_bearing}

\title{
Add bearings
}

\description{
Bearing towards the next coordinate in the tracks.
}

\usage{
t_bearing(
  tracks, t_id = "id", bind = TRUE, drop = TRUE, cname = "bearings"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
}

\details{
  For the last coordinate within a track the function returns \code{empty} because there are no bearings towards a following coordinate possible.

  If the succeeding coordinate is the same like the current coordinate, the function return \code{empty} for the current coordinate.
}

\note{
  Please be aware that this function calculates the initial bearing from the first to the second point and that this bearing is saved with the second point. This seems counter intuitive for an initial bearing but is done for better compatibility with the gap functions.
}

\value{
  \code{\link{psyo}}
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link[geosphere]{bearing}}, \code{\link{t_distance}}, \code{\link{t_speed}}, \code{\link{t_time_difference}}
}

\examples{\dontrun{
data(psyo)
psyo <- t_bearing(psyo)
}}
