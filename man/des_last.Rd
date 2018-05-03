\name{des_last}

\alias{des_last}

\title{
Get last value within each track
}

\description{
Get last value within each track
}

\usage{
des_last(
  tracks, ctarget, cgaps = "", cname = "last", drop = TRUE, t_id = "id",
  des_df = ""
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{ctarget}{\code{character}. Column name of \code{tracks} that contains the variable for the calculation.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
  \item{cname}{\code{character}. Column name of the returned calculation result.}
  \item{drop}{\code{logical}. If \code{TRUE} and only one observation is returned drop the \code{data frame} and collapse the return value to a \code{vector}. }
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{des_df}{\code{data frame}. Function results will be merge with this data frame.}
}

\value{
Data frame
  \item{t_id}{unique id of the track}
  \item{cname}{calculated result of the track}
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
  \code{\link{des_summary}}, \code{\link{des_duplicates}}, \code{\link{des_first}}, \code{\link{des_length}}, \code{\link{des_max}}, \code{\link{des_min}}, \code{\link{des_mean}}, \code{\link{des_sd}}, \code{\link{des_sum}}
}

\examples{
data(psyo_rounds2)
last <- des_last(psyo_rounds2, "p_id")
}

