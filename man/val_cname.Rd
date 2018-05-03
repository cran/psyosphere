\name{val_cname}

\alias{val_cname}

\title{
Validate the column name of a data frame
}
\description{
Validate the column name of a data frame
}

\usage{
val_cname(tracks, cname, type = "", size = 0, force = 2, def = TRUE)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{cname}{\code{character}. Column name of column in \code{tracks} that is to be validated.}
  \item{type}{\code{character}. Type of column in \code{tracks} determined by \code{\link[base]{mode}}.}
  \item{size}{\code{size}. Observation count of column in \code{tracks} determined by \code{\link[base]{length}}.}
  \item{force}{\emph{numeric}. An error with \code{force_id} will be reported as \link[base]{stop} when \code{2}, \link[base]{warning} when \code{1} or nothing when \code{0}.}
  \item{def}{\emph{logical}. Ignore this check if \code{cname = ""}.}
}

\value{
  \code{character}
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\seealso{
  \code{\link{val_cname}},\code{\link{val_psyo}}, \code{\link{val_var}}
}

\examples{\donttest{
data(psyo)

# Test t_id
t1 <- psyo
t_id <- "id"
e <- val_cname(t1, t_id, size = 15, type = "numeric"); if (e != "") {stop(e)}

# Be aware that id column is saved as "factor" and therefore mode() returns
# numeric.

t2 <- psyo
t_id <- 1
e <- val_cname(t2, t_id); if (e != "") {stop(e)}

t3 <- psyo
t_id <- "id"
e <- val_cname(t3, t_id, size = 15, type = "character"); if (e != "") {stop(e)}

t4 <- psyo
t_id <- "id"
e <- val_cname(t4, t_id, size = 20); if (e != "") {stop(e)}

t5 <- psyo
t_id <- "id"
e <- val_cname(t5, t_id, size = "20"); if (e != "") {stop(e)}
}}
