\name{plot_line}

\alias{plot_line}

\title{
Plot line on map
}

\description{
Adds a line to an existing \code{\link{ggmap}} object.
}

\usage{
plot_line(
  line, colour = "", size = 1, plot = "", zoom = -1
)
}

\arguments{
  \item{line}{\emph{list}. A list with the column lon (\emph{numeric}) and lat (\emph{numeric}).}
  \item{colour}{\emph{character}. Colour of the line.}
  \item{size}{\emph{numeric}. Thickness of the line.}
  \item{plot}{\code{\link{ggmap}}. An existing map / plot where the tracks are added. If no plot is provided a Google map will be downloaded.}
  \item{zoom}{\emph{numeric}. Zoom factor of the map. See \code{\link[ggmap]{get_googlemap}.}}
}

\value{
A \code{\link{ggmap}} object.
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{plot_map}}, \code{\link{plot_tracks}}, \code{\link{plot_polygon}}
}

\examples{\donttest{\donttest{
finish <- data.frame(lon = c(6.851810,6.851000), lat = c(52.241800,52.240300))
plot_line(finish)
}}}
