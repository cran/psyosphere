\name{average_coordinates}

\alias{average_coordinates}

\title{
Calculates the mean position of coordinates within a time interval.
}

\description{
  Calculates the mean position of coordinates within a time interval.
}

\usage{
average_coordinates(tracks, num, units = "minutes", t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{num}{\emph{num} The amount of time.}
  \item{units}{\emph{char} Unit for time measurement. For example: "seconds", "minutes" or "days". See \code{\link[lubridate]{duration}} for more details.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\details{
This function can be used to compress huge amounts of data to speed up the calculations.

The average for the columns time, lon and lat is calculated. See \code{\link[geosphere]{geomean}} for details. Other columns will be preserved but only the first row of every interval is considered.
}

\value{
\emph{data frame}
  \item{lon}{\emph{num} averaged longitude }
  \item{lat}{\emph{num} averaged latitude }
  \item{time}{\emph{POSIXct} averaged time }
  \item{other}{Preserved columns from input}
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
  \code{\link[geosphere]{geomean}}, \code{\link{average_duplicates}}, \code{\link{select_test_sample}}
}

\examples{
data(psyo_geomean)
average_coordinates(psyo_geomean,30,"seconds")
}
\keyword{ compress }
\keyword{ geomean }
