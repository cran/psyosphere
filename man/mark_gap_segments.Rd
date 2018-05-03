\name{mark_gap_segments}

\alias{mark_gap_segments}

\title{
Adding column with segment names between gaps
}

\description{
Adding column with segment names between gaps
}

\usage{
mark_gap_segments(
  tracks, cgaps, bind = TRUE, drop = TRUE, cname = "gap_segments", t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of new column in \code{tracks} that contains the segment names.}
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
  \code{\link{mark_speed_gaps}}, \code{\link{mark_time_gaps}}
}

\examples{
data(psyo_rounds2)
psyo_rounds2 <- average_duplicates(psyo_rounds2)
psyo_rounds2 <- t_time_difference(psyo_rounds2)

psyo_rounds2 <- mark_time_gaps(psyo_rounds2)
psyo_rounds2 <- mark_gap_segments(psyo_rounds2,"time_gap")
}
