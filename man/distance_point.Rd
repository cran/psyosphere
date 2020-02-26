\name{distance_point}

\alias{distance_point}

\title{
Add the distances to a point from each coordinate
}

\description{
Add the distances to a point from each coordinate
}

\usage{
distance_point(
  tracks, point, bind = TRUE, drop = TRUE, cname = "dis_to_point_in_m"
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{point}{\emph{list}. A list, matrix or data.frame with the columns lon (\emph{numeric}) and lat (\emph{numeric}) and one row.}
  \item{bind}{\emph{logical}. Return the distance as list (\code{FALSE}) or add it to \code{tracks} (\code{TRUE}).}
  \item{drop}{\code{logical}. If \code{TRUE} drop the \code{data frame} and return value as \code{vector} or \code{list}. }
  \item{cname}{\code{character}. Column name of the returned calculation result.}
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
	\code{\link{distance_line}},\code{\link{distance_peers}}, \code{\link{distance_psyo}}
}

\examples{\donttest{
data(psyo)

# Un-named list
point <- c(4.936197, 52.314701)
distance_point(psyo[1,], point, bind = FALSE)

# Named list
point <- c(lat = 52.314701, lon = 4.936197)
distance_point(psyo[1,], point, bind = FALSE)

# Multiple distance to point
coordinates <- distance_point(psyo, point)
}}
