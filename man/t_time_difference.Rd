\name{t_time_difference}

\alias{t_time_difference}

\title{
Add time difference column for weighted statistics
}

\description{
This functions generates a column with the time difference to the next coordinate. This is important since the GPS variables seldom have the same time difference and means and standard deviations should be weighted. The generated column can be used as "weight" variable.
}

\usage{
t_time_difference(
  tracks, units = "secs", bind = TRUE, drop = TRUE, cname = "time_difference",
  t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{units}{\code{character}. Same as for \code{link[base]{difftime}} but avoid using "auto". Auto could generate different \code{units} for the different tracks.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\value{
  \code{\link{psyo}}
}

\note{
  The time difference between the first and the second point is stored with the second point. This is done for higher compatibility with the gap functions.
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
	\code{\link[base]{difftime}}, \code{\link{t_bearing}},\code{\link{t_distance}}, \code{\link{t_speed}}
}

\examples{\dontrun{
data(psyo)
t_time_difference(psyo, units = "secs")
}}
