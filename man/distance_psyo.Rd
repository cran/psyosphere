\name{distance_psyo}

\alias{distance_psyo}

\title{
Add distance to another track in psyo format
}

\description{
Add distance to another track in \code{\link{psyo}} format.
}

\usage{
distance_psyo(
  tracks1, tracks2, t_id1 = "id", t_id2 = "id", bind = TRUE, drop = TRUE
)
}

\arguments{
  \item{tracks1}{\code{\link{psyo}}. Data frame with tracks.}
  \item{tracks2}{\code{\link{psyo}}. Data frame with tracks.}
  \item{t_id1}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{t_id2}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
}

\value{
  \code{\link{psyo}}
}

\note{
  Be aware: this function can take a lot time. You can use \code{\link{average_coordinates}} first to test your script with a small sample.
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
	\code{\link{distance_line}},\code{\link{distance_peers}}, \code{\link{distance_point}}
}

\examples{\donttest{\donttest{
data(psyo_rounds2)
psyo_distance <- select_test_sample(psyo_rounds2, 5)
psyo_distance <- distance_psyo(psyo_rounds2, psyo_rounds2)
}}}
