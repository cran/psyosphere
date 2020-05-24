\name{about common mistakes}

\alias{about_common_mistakes}

\title{
Tips to prevent common mistakes
}

\description{
Tips to prevent common mistakes
}

\details{

  \describe{
    \item{\bold{Forgetting to use weighted statistics.}}{Mostly the coordinates don't have an even time interval. This can be because of the missing data, planned data gaps or deviations in the GPS tracker. To prevent this, you can calculate the time difference between coordinates with \code{\link[base]{difftime}} and used it as weight for weighted descriptive statistics.}
    \item{\bold{Recalculating track data after removing gaps.}}{After removing gaps, you should be careful to recalculate speed, time difference, etc. since this function can't see that the gaps are removed. To work around this, you can just omit gaps with the descriptive functions that begin with "des_". They can ignore gaps.}
  }

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
	\code{\link{about_analysing_tips}}, \code{\link{about_demos}}, \code{\link[base]{difftime}}, \code{\link[Hmisc]{wtd.mean}}
}

\examples{\dontrun{
# Example forgetting to use weighted statistics --------------------------------
data(psyo)
tracks <- t_speed(psyo)
tracks <- t_time_difference(tracks, units = "secs")

# Without weighted statistics
mean(tracks[,c("speed")], na.rm = TRUE)
sd(tracks[,c("speed")], na.rm = TRUE)

# With weighted statistics
des_mean(tracks, "speed", "time_difference", t_id = "")

Hmisc::wtd.mean(
  tracks[,c("speed")], as.numeric(tracks[,c("time_difference")])
)

sqrt(Hmisc::wtd.var(
  tracks[,c("speed")], as.numeric(tracks[,c("time_difference")])
))
}}
