\name{plot_map}

\alias{plot_map}

\title{
Get a Google map
}

\description{
Get a Google map that fits to the tracks that are provided in \code{\link{psyo}} format.
}

\usage{
plot_map(
  tracks, zoom = -1, maptype = "terrain", extent = "panel"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{zoom}{\emph{numeric}. Zoom level. -1 for auto zoom or from 3 (continent) to 21 (building). See also ggmap::get_map.}
  \item{maptype}{\code{character}. See ggmap::get_googlemap.}
  \item{extent}{\code{character}. See ggmap::ggmap.}
}

\value{
  A ggmap object.
}

\author{
Benjamin Ziepert
Dr. Elze Ufkes
}

\seealso{
  \code{\link{plot_line}}, \code{\link{plot_tracks}}, \code{\link{plot_polygon}}
}

\examples{\dontrun{\dontrun{
data(psyo)
plot_map(psyo)
}}}
