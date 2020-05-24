
\name{plot_tracks}

\alias{plot_tracks}

\title{
Plot tracks on a map
}

\description{
Plot tracks in the \code{\link{psyo}} format on map.
}

\usage{
plot_tracks(
  tracks, single = FALSE, line = TRUE, dots = TRUE, plot = "", zoom = -1,
  save_dir = "", cgaps = "", t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Tracks that will be plotted.}
  \item{single}{\emph{logical}. Display the plotted maps. Between each plot the script will wait for user confirmation until the next plot will be displayed.}
  \item{line}{\code{logical}. Whether to draw a line between the coordinates.}
  \item{dots}{\code{logical}. Whether to display the coordinates.}
  \item{plot}{ggmap. An existing map / plot where the tracks are added. If no plot is provided a Google map will be downloaded.}
  \item{zoom}{\emph{numeric}. Zoom factor of the map. See ggmap::get_googlemap.}
  \item{save_dir}{\emph{character}. Save the plots in a directory. If empty no plots will be saved.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
  \item{t_id}{\emph{character} or \emph{integer}. The column name that identifies the different tracks. Use t_id = "" to process all tracks.}
}

\value{
A ggmap object.
}

\author{
Benjamin Ziepert
}

\seealso{
  \code{\link{plot_map}}, \code{\link{plot_line}}, \code{\link{plot_polygon}}
}

\examples{\dontrun{\dontrun{
# Plot tracks
data(psyo_rounds2)
plot <- plot_tracks(psyo_rounds2)
plot

# Get zoom level
plot$zoom
}}}
