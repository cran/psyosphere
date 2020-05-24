\name{mark_speed_gaps}

\alias{mark_speed_gaps}

\title{
Mark speeds that exceed a certain speed limit as gaps
}

\description{
Mark speeds that exceed a certain speed limit as gaps
}

\usage{
mark_speed_gaps(
  tracks, speed_limit, cspeed = "speed", bind = TRUE, drop = TRUE,
  cname = "speed_gap", t_id = "id"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{speed_limit}{\code{numeric}. Values in column \code{cspeed} that are equal or higher than this value will be marked as gaps in column \code{cgaps} as \code{TRUE}.}
  \item{cspeed}{\code{character}. Column name of \code{tracks} that contains the speed as \code{numeric} values.}
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
  \code{\link{mark_gap_segments}}, \code{\link{mark_time_gaps}}, \code{\link{select_gaps}}
}
\examples{\dontrun{
# Get example data
data(psyo_rounds2)
speedt <- psyo_rounds2[ c(1:5,11:15) ,]
remove(psyo_rounds2)

# clean-up data
speedt <- average_duplicates(speedt)
speedt <- t_time_difference(speedt)
speedt <- mark_time_gaps(speedt)

# Add speed
speedt <- t_speed(speedt)

# Advanced mean speed without speed gap
des_mean(speedt, "speed", "time_difference", "time_gap")

# Mark speed gap
speedt <- mark_speed_gaps(speedt, 8)

# Advanced mean speed with speed gap
des_mean(speedt, "speed", "time_difference", c("time_gap","speed_gap"))
}}
