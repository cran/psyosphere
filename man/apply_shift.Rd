\name{apply_shift}

\alias{apply_shift}

\title{
Copy columns and offset the index of the copied column
}

\description{
Copy columns and offset the index of the copied column
}

\usage{
apply_shift(tracks, factor = 1, bind = TRUE, csubset = "", t_id = "id")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{factor}{\code{character} or \code{numeric}. Number of shifted copied to be created. With - or + a direction can be indicated. For instance, +1 copies the value of the following coordinate. -1 copies the value of the previous coordinate.}
  \item{bind}{\code{logical}. Whether to bind the row to \code{tracks} or to return it as separate column.}
  \item{csubset}{\code{list}. A list of column names in \code{tracks} that will be copied.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
}

\value{
  \code{\link{psyo}}
}

\note{
  This function drops sometimes attributes.
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
  \code{\link{psyo}},\code{\link{apply_tracks}}
}

\examples{\dontrun{
data(psyo)
apply_shift(psyo, csubset = c("lon", "lat"))
}}
