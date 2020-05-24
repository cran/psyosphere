\name{average_duplicates}

\alias{average_duplicates}

\title{
Correct coordinates with the same time
}

\description{
Correct coordinates with the same time by replacing with one coordinate with a geomean position.
}

\usage{
average_duplicates(tracks, t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
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
  \code{\link[geosphere]{geomean}}, \code{\link{average_coordinates}}
}

\examples{\dontrun{\dontrun{
# Get data
data(psyo)
psyo <- psyo[c(1,15),]
psyo[2, c("time","id")] <- psyo[1, c("time","id")]

# Plot coordinates
map <- plot_map(psyo)
plot <- map + ggplot2::geom_point(data = psyo, size = 5)
plot <- plot_line(psyo[,c("lon", "lat")], plot = plot)
plot

# Calculate mean position
psyo <- average_duplicates(psyo)

# Plot mean position
plot + ggplot2::geom_point(data = psyo, size = 5)

}}}
