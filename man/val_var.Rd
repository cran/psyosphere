\name{val_var}
\alias{val_var}
\title{
Validate variables
}
\description{
Validates variables before further procedure execution.
}
\usage{
val_var(test_var, type, force = 2, size = 0, def = FALSE)
}
\arguments{
  \item{test_var}{\emph{Multiple.} The variable that is to be tested.}
  \item{type}{\emph{Character.} The variable type determined by \code{\link[base]{mode}} or \code{lubridate::is.POSIXct}. For example \emph{numeric}, \emph{character}, \emph{logical}, \emph{list}, \emph{POSIXct} or \emph{ggplot}.}
  \item{force}{\emph{numeric.} Error message is sent as warning (\emph{1}) or stop (\emph{2}).}
  \item{size}{\emph{numeric.} If \code{size} is not 0 the length of \code{test_var} will be checked with \code{size}.}
  \item{def}{\emph{logical}. Ignore this check if \code{cname = ""}.}
}
\author{
Benjamin Ziepert
}

\seealso{
  \code{\link{val_cname}},\code{\link{val_psyo}}, \code{\link{val_var}}
}

\examples{\dontrun{
# Create variables
id <- 10
name <- "test"
time <- as.POSIXct("1986-08-31 02:15:00")

# Check variables
# e <- val_var(id, "character"); if (e != "") {stop(e)} # error and stop
# e <- val_var(name, "logical", FALSE); if (e != "") {stop(e)} # error and warning
e <- val_var(time, "POSIXct"); if (e != "") {stop(e)} # no error

}}

