\name{distance_line}

\alias{distance_line}

\title{
Add shortest distance to a line
}

\description{
Add shortest distance to a line
}

\usage{
distance_line(
  tracks, line, bind = TRUE, drop = TRUE, cname = "distances_to_line"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{line}{\emph{list}. A list with the column lon (\emph{numeric}) and lat (\emph{numeric}).}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
}

\value{
  \code{\link{psyo}} or \code{\link{list}}. Distance in meter.
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
	\code{\link{distance_peers}}, \code{\link{distance_point}}, \code{\link{distance_psyo}}
}

\examples{\dontrun{\dontrun{
# Get data
data(psyo_rounds)

# Set start and finish
lon <- c(6.849975, 6.849627, 6.850001, 6.850350, 6.849975)
lat <- c(52.241745, 52.241100, 52.241004, 52.241649, 52.241745)
polygon_start <- data.frame(lon, lat)
remove(lon, lat)

lon <- c(6.851810, 6.851000, 6.851489, 6.852296, 6.851810)
lat <- c(52.241800, 52.240300, 52.240163, 52.241657, 52.241794)
polygon_finish <- data.frame(lon, lat)
remove(lon, lat)

# Select between start and finish
psyo_rounds <- select_between_polygons(
  psyo_rounds, polygon_start, polygon_finish
)

# Finish line
finish <- data.frame(lon = c(6.851810,6.851000), lat = c(52.241800,52.240300))

# Plot tracks, selection polygons and finish line
plot <- plot_tracks(psyo_rounds, t_id = "")
plot <- plot_polygon(polygon_start, plot = plot)
plot <- plot_polygon(polygon_finish, plot = plot)
plot_line(finish, plot = plot)

# Add distance to line to dataframe
psyo_rounds <- distance_line(psyo_rounds,finish, TRUE)
}}}
