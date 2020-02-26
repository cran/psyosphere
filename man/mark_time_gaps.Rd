\name{mark_time_gaps}

\alias{mark_time_gaps}

\title{
Mark segments between data gaps
}

\description{
Mark segments between data gaps
}

\usage{
mark_time_gaps(
  tracks, interval = 0, factor = 3, ctime_difference = "time_difference",
  bind = TRUE, drop = TRUE, cname = "time_gap", t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{interval}{\code{numeric}. Recording interval of the GPS tracker in seconds. Use 0 to automatically determine the interval. For this the most frequent interval is used.}
  \item{factor}{\code{numeric}. Multiplier to determine gaps. If a time difference between coordinates is bigger than \code{tracker_interval * factor} it is marked as gap.}
  \item{ctime_difference}{\code{character}. Column name of \code{tracks} that contains the time difference as \code{numeric} values.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{cname}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}. If the column does not exist it will be created.}
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
  \code{\link{mark_gap_segments}}, \code{\link{mark_speed_gaps}}, \code{\link{select_gaps}}
}

\examples{\donttest{
# Get example data
data(psyo_rounds2)

# clean-up data
psyo_rounds2 <- average_duplicates(psyo_rounds2)

# Add gap segments
psyo_rounds2 <- t_time_difference(psyo_rounds2)
psyo_rounds2 <- mark_time_gaps(psyo_rounds2)

# Check result
psyo_rounds2 <- psyo_rounds2[ which(psyo_rounds2[,"time_gap"]) ,]
}}
