\name{apply_tracks}

\alias{apply_tracks}

\title{
Run function on each track in a psyo data frame
}

\description{
Run function on each track in a \code{\link{psyo}} data frame. The function is in form of a \emph{character} expression.
}

\usage{
apply_tracks(tracks, exp,
  arg1 = "", arg2 = "", arg3 = "", arg4 = "",arg5 = "", arg6 = "", arg7 = "",
  arg8 = "", arg9 = "", t_id = "id", info = FALSE
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame containing tracks in \code{\link{psyo}} format.}
  \item{exp}{\emph{character}. The function and arguments that will be evaluated.}
  \item{arg1}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg2}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg3}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg4}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg5}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg6}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg7}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg8}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{arg9}{\emph{multiple}. Arguments that will be sent to the target function.}
  \item{t_id}{Unique by time sorted ID for every coordinate within a track. Use \code{t_id = ""} to make no selection but take all data.}
  \item{info}{\emph{logical}. Measures the time consumption for each track calculation.}
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
Benjamin Ziepert
}

\section{Warning }{

  \bold{Don't use this function for 'psyosphere' main functions.} Most of the 'psyosphere' functions have \code{apply_tracks()} already included. By adding it again you can get strange results or break the function.

  \bold{Only return the changed "eval_track" as result.} The function is splitting a \code{\link{psyo}} data frame in sub tracks. After changes are applied that sub tracks or merged together again. Therefore, it is important to only work in the sub track. If for instance every time the \code{\link{psyo}} data frame is returned by the evaluated function than the data frame gets stacked again and again on top of itself. See the examples below for how this can look like.
}

\seealso{
  \code{\link{psyo}}, \code{\link{apply_shift}}
}
\examples{\dontrun{
# Working examples -------------------------------------------------------------

# Test function for examples
test_sum <- function(track, more = 0) {
  track$lon_sum <- sum(track$lon) + more
  return(track)
}

# Simple example
data(psyo)
psyo <- apply_tracks(
  psyo,"test_sum(eval_track)"
)

# See all data as one track
data(psyo)
psyo <- apply_tracks(
  psyo,"test_sum(eval_track)", t_id =""
)

# Use of arguments
data(psyo)
psyo <- apply_tracks(
  psyo,"test_sum(eval_track, arg1)", arg1 = 5
)

# What not to do ---------------------------------------------------------------

# Only return the changed "eval_track" as result. The following examples show
# what can go wrong otherwise.

test_wrong <- function(selected_track, all_tracks) {
  all_tracks$sum <- all_tracks$lon + all_tracks$lat
  return(all_tracks)
}

data(psyo)
psyo <- psyo[psyo[,c("p_id")]== 0,]

psyo <- apply_tracks(
  psyo,"test_wrong(eval_track, arg1)", arg1 = psyo
)

}}
