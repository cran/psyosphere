\name{plot_polygon}

\alias{plot_polygon}

\title{
Plot polygon on map
}

\description{
Adds a polygon to an existing ggmap object.
}

\usage{
plot_polygon(polygon, colour = "blue", plot = "", zoom = -1)
}

\arguments{
  \item{polygon}{\emph{list}. A list with the column lon (\emph{numeric}) and lat (\emph{numeric}).}
  \item{colour}{\emph{character}. Colour of the line.}
  \item{plot}{ggmap. An existing map / plot where the tracks are added. If no plot is provided a Google map will be downloaded.}
  \item{zoom}{\emph{numeric}. Zoom factor of the map. See ggmap::get_googlemap.}
}

\value{
A ggmap object.
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{plot_map}}, \code{\link{plot_line}}, \code{\link{plot_tracks}}
}

\examples{\dontrun{\dontrun{
lon <- c(6.849975, 6.849627, 6.850001, 6.850350, 6.849975)
lat <- c(52.241745, 52.241100, 52.241004, 52.241649, 52.241745)
polygon <- data.frame(lon, lat)
remove(lon, lat)

plot <- plot_polygon(polygon)
plot
}}}
