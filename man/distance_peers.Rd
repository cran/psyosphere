\name{distance_peers}

\alias{distance_peers}

\title{
Add distance to peers
}

\description{
Add distance in meters to peers within the same selection
}

\usage{
distance_peers(
  tracks, cpeer = "", single = FALSE, average = TRUE, cname = "average_dis",
  bind = TRUE, drop = TRUE, t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{cpeer}{\emph{character}. Column that identifies peers.}
  \item{single}{\emph{logical}. Append distances to each participant independently.}
  \item{average}{\emph{logical}. Append average distances to peers.}
  \item{cname}{\code{character}. Column name for the average distance.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the peers that get compared with each other}
}

\details{
  The function also determines the distance if the time stamps of the coordinates don't match. Please look at the example section for the details.

  Only the distance to peers is determined. Therefore, distance to one-self is \code{NA}.
}

\value{
  \code{\link{psyo}}
}

\note{
  Be aware: this function can take a lot time. You can use \code{\link{average_coordinates}} first to test your script with a small sample.
}

\section{Credit}{
  If you use 'psyosphere' for commercial use or research, please support us by include one off the following references:
  \itemize{
    \item{\bold{MIT license:} "psyosphere" by B. Ziepert, E. G. Ufkes & P. W. de Vries from https://CRAN.R-project.org/package=psyosphere}
    \item{\bold{APA}: Ziepert, B., Ufkes, E., & de Vries, P. W. (2018). psyosphere: Analyse GPS Data. Retrieved from https://CRAN.R-project.org/package=psyosphere}
    \item{\bold{APA}: Vries, P., Ziepert, B., & Ufkes, E. (2016). "De psychologie van bewegingen GPS-technologie voor de analyse van natuurlijk gedrag." Tijdschrift voor Human Factors 2: 11-15.}
  }
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
	\code{\link{distance_line}}, \code{\link{distance_point}}, \code{\link{distance_psyo}}
}

\examples{\dontrun{\dontrun{
# Simple example ---------------------------------------------------------------
data(psyo_rounds2)
time <- as.POSIXct("2015-09-03 13:51:07")
tracks <- psyo_rounds2[ psyo_rounds2[,"time"] == time, ]
tracks <- distance_peers(tracks)

# Example with intersect position ----------------------------------------------

#  p1 --------x-------> p2
#             ^
#             |
#             |
#            p3

# We have two tracks. From track 1 we have p1 and p2 at a certain time. From
# tracks 2 we have point 3. Point 3 has a time between p1 and p2. We want to
# know what the distance from point 3 is to track 1 at the time of point 3. For
# this we need determine point x.

# Create the points
p1 <- data.frame(time = "2016-01-01 01:00:00", lon = 0, lat = 0, id = "1")
p2 <- data.frame(time = "2016-01-01 03:00:00", lon = 2, lat = 0, id = "1")
p3 <- data.frame(time = "2016-01-01 02:00:00", lon = 1, lat = 1, id = "2")
p1$time <- as.POSIXct(p1$time)
p2$time <- as.POSIXct(p2$time)
p3$time <- as.POSIXct(p3$time)

# Combine into a track
tracks <- rbind(p1, p2, p3)

# Get point x for illustration
x <- psyosphere:::timed_destination_point_private(p1, p2, p3, "id")

# Plot points as track for illustration
plot <- plot_tracks(tracks)
plot

# Add x to plot for illustration
plot_tracks(x, plot = plot)

# Get distances
tracks <- distance_peers(tracks)
}}}

