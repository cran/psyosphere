\name{distance_to_direct_line}

\alias{distance_to_direct_line}

\title{
Add deviation from shortest route from begin of track to a line
}

\description{
Add deviation from shortest route from begin of track to a line
}

\usage{
distance_to_direct_line(
  tracks, line, bind = TRUE, drop = TRUE, cname = "distance_to_direct_line",
  t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{line}{\emph{list}. A list with the column lon (\emph{numeric}) and lat (\emph{numeric}).}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
}


\value{
  \code{\link{psyo}}. Distance in meter.
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
	\code{\link{distance_line}}
}

\examples{\donttest{\donttest{
# Get data
data(psyo_rounds2)

# Create finish line
finish <- data.frame(lon = c(6.851810,6.851000), lat = c(52.241800,52.240300))

# Plot tracks and finish
plot <- plot_tracks(psyo_rounds2, t_id = "")
plot_line(finish, plot = plot)

# Get deviation from shortest rout from begin to finish
psyo_rounds2 <- distance_to_direct_line(psyo_rounds2, finish)
}}}
