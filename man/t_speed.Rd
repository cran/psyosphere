\name{t_speed}

\alias{t_speed}

\title{
Add speed
}

\description{
Speed towards the next coordinate in the track in kmh.
}

\usage{
t_speed(
  tracks, bind = TRUE, drop = TRUE, cname = "speed", t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}
\details{
For the last coordinate within a track the function returns \code{NA} because there is no speed towards a following coordinate possible.
}

\value{
  \code{\link{psyo}}
}

\note{
  The speed between the first and the second point is stored with the second point. This is done for higher compatibility with the gap functions.
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{t_bearing}},\code{\link{t_distance}}, \code{\link{t_time_difference}}
}

\examples{\donttest{
data(psyo)
psyo <- t_speed(psyo)
}}
