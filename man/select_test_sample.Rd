\name{select_test_sample}

\alias{select_test_sample}

\title{
Select a sample from each track
}

\description{
Select a sample from each track to test functions quicker.
}

\usage{
select_test_sample(tracks, size = 3, t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{size}{\emph{numeric}. Remaining number of coordinates of each track in \code{tracks}}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\author{
Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{average_coordinates}}
}

\examples{\dontrun{
data(psyo)
test_tracks <- select_test_sample(psyo)
}}
