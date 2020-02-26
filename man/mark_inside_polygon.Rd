\name{mark_inside_polygon}

\alias{mark_inside_polygon}

\title{
Mark coordinates within a polygon
}

\description{
A column will be created that indicates whether a coordinate lies within a polygon or not. See also \code{\link[sp]{point.in.polygon}}.
}

\usage{
mark_inside_polygon(
  tracks, polygon, bind = TRUE, drop = TRUE, cname = "in_polygon"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{polygon}{\emph{list}. A list with the column lon (\emph{numeric}) and lat (\emph{numeric}).}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. The name of the new column.}
}

\value{
  \code{\link{psyo}}
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
	\code{\link[sp]{point.in.polygon}}
}

\examples{\donttest{\donttest{
# Create polygon
lon <- c(4.92, 4.93, 4.93, 4.92, 4.92)
lat <- c(52.311, 52.311, 52.308, 52.308, 52.311)
poly <- data.frame(lon, lat)
remove(lon, lat)

# Get data
data(psyo)

# Plot polygon and data
plot <- plot_tracks(psyo, t_id = "")
plot <- plot_polygon(poly, plot = plot)
plot

# Mark coordinates within plygon
psyo <- mark_inside_polygon(psyo, poly)

# Plot inside polygon in different color
in_poly <- psyo[ psyo[,"in_polygon"] != 0,]
in_poly[,"dot_color"] <- "red"
plot_tracks(in_poly, plot = plot)
}}}
