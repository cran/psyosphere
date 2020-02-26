\name{select_between_polygons}

\alias{select_between_polygons}

\title{
Select tracks between two polygons.
}

\description{
Select tracks between a start and a finish polygon. Only the data between the polygons will remain. Data that is not between the start and finish polygon will be disregarded. If a track passes multiple times first the start and then the finish will be split up in rounds and new track id's will be created for each round.
}

\usage{
select_between_polygons(
  tracks, poly1, poly2, t_id = "id", merge_id = TRUE
)
}

\arguments{
    \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{poly1}{\emph{data frame}. A data frame with the columns \emph{lon} (\emph{numeric}) and \emph{lat} (\emph{numeric}). All coordinates will be selected that start after leaving this polygon and enter \emph{polygon_finish}. The polygon should be closed, therefore the first and last coordinate must be the same. See also \code{\link[sp]{point.in.polygon}}.}
  \item{poly2}{\emph{data frame}. A data frame with the columns \emph{lon} (\emph{numeric}) and \emph{lat} (\emph{numeric}). All coordinates will be selected that start after leaving \code{polygon_start} and enter this polygon. The polygon should be closed, therefore the first and last coordinate must be the same. See also \code{\link[sp]{point.in.polygon}}.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{merge_id}{\emph{logical}. If \code{TRUE} append the round to the current track id column \code{t_id}. If \code{FALSE} create a separate column with the round number.}
}

\details{

The following image shows a track before selection.

\if{html}{\figure{rounds1.png}{Before selection}} \if{latex}{\figure{rounds1.png}{options: width = 0.5in}}

The following image shows a track after selection with 3 rounds.

\if{html}{\figure{rounds2.png}{After selection}} \if{latex}{\figure{rounds2.png}{options: width = 0.5in}}

}

\author{
Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\examples{\donttest{\donttest{
# Create polygons
lon <- c(6.849975, 6.849627, 6.850001, 6.850350, 6.849975)
lat <- c(52.241745, 52.241100, 52.241004, 52.241649, 52.241745)
polygon_start <- data.frame(lon, lat)
remove(lon, lat)

lon <- c(6.851810, 6.851000, 6.851489, 6.852296, 6.851810)
lat <- c(52.241800, 52.240300, 52.240163, 52.241657, 52.241794)
polygon_finish <- data.frame(lon, lat)
remove(lon, lat)

# Get a track
data(psyo_rounds)

# Plot tracks
plot <- plot_tracks(psyo_rounds, zoom = 17, t_id = "")
plot

# Add start and finish polyon
plot <- plot_polygon(polygon_start, plot = plot)
plot_polygon(polygon_finish, plot = plot)

# Select data between polygon
psyo_rounds <- select_between_polygons(
  psyo_rounds, polygon_start, polygon_finish
)

# Plot the remaining data
plot <- plot_tracks(psyo_rounds, t_id = "")
plot <- plot_polygon(polygon_start, plot = plot)
plot_polygon(polygon_finish, plot = plot)
}}}
