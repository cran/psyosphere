\name{des_mean}

\alias{des_mean}

\title{
  Calculate normal and weighted means while excluding gaps in data
}
\description{
  Calculate normal and weighted means while excluding gaps in data
}
\usage{
des_mean(
  tracks, ctarget, cweight = "", cgaps = "", cname = "mean", drop = TRUE,
  t_id = "id", des_df = ""
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{ctarget}{\code{character}. Column name of \code{tracks} that contains the variable for the calculation.}
  \item{cweight}{\code{character}. Column name of \code{tracks} that contains the weight for the calculation.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
  \item{cname}{\code{character}. Column name of the returned calculation result.}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{des_df}{\code{data frame}. Function results will be merge with this data frame.}
}

\value{
Data frame
  \item{id}{id of the track}
  \item{mean}{calculated mean of the track}

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
  \code{\link{des_summary}}, \code{\link{des_duplicates}}, \code{\link{des_first}}, \code{\link{des_last}}, \code{\link{des_length}}, \code{\link{des_max}}, \code{\link{des_min}}, \code{\link{des_sd}}, \code{\link{des_sum}}
}

\examples{\donttest{
# Get example data
data(psyo_rounds2)
psyo_rounds2 <- psyo_rounds2[ c(1:5,11:15) ,]

# clean-up data
psyo_rounds2 <- average_duplicates(psyo_rounds2)

# Add gap segments
psyo_rounds2 <- t_time_difference(psyo_rounds2)
psyo_rounds2 <- mark_time_gaps(psyo_rounds2)

# Add speed
psyo_rounds2 <- t_speed(psyo_rounds2)

# Calculate different means
normal <- des_mean(psyo_rounds2, "speed", cname = "normal")

weighted <- des_mean(
  psyo_rounds2, "speed", cweight = "time_difference", cname = "weighted"
)

segmented <- des_mean(
  psyo_rounds2, "speed", cgaps= "time_gap", cname = "segmented"
)

segmented_weighted <- des_mean(
  psyo_rounds2,
  "speed",
  cweight = "time_difference",
  cgaps = "time_gap",
  cname = "segmented_weighted"
)

}}
