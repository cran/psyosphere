\name{val_psyo}
\alias{val_psyo}
\title{
Validate psyo format
}
\description{
Checks if the provided data frame is conforming to the format that is used by 'psyosphere' and returns a \code{\link[base]{warning}} or \code{\link[base]{stop}} if necessary.
}
\usage{
val_psyo(tracks, id = 1, p_id = 1, time = 1, lon = 2, lat = 2)
}
\arguments{
  \item{tracks}{\emph{psyo}. The data frame that is to be check if it confirms to the \code{\link{psyo}} format.}

  \item{id}{\emph{numeric}. An error with \code{force_id} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}

  \item{p_id}{\emph{numeric}. An error with \code{force_p_id} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}

  \item{time}{\emph{numeric}. An error with \code{force_time} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}

  \item{lon}{\emph{numeric}. An error with \code{force_lon} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}

  \item{lat}{\emph{numeric}. An error with \code{force_lat} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}

}
\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{val_cname}},\code{\link{val_psyo}}, \code{\link{val_var}}
}

\examples{\donttest{\donttest{
# Produce a warning ------------------------------------------------------------
data(psyo)
psyo$time <- NULL # remove time column
e <- val_psyo(psyo); if (e != "") {stop(e)}

# Produce a stop ---------------------------------------------------------------
data(psyo)
psyo$time <- NULL # remove time column
e <- val_psyo(psyo, time = 2); if (e != "") {stop(e)}

# Produce a stop without setting "force" ---------------------------------------
data(psyo)
psyo$lon <- NULL # remove time column
e <- val_psyo(psyo); if (e != "") {stop(e)}
}}}
