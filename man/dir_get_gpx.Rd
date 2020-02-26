\name{dir_get_gpx}

\alias{dir_get_gpx}

\title{
Read GPX files from directory into data frame.
}

\description{
Read GPX files from directory into data frame.
}

\usage{
dir_get_gpx(dir, tz = "", stringsAsFactors = default.stringsAsFactors())
}

\arguments{
  \item{dir}{\emph{character}. The directory of the GPX files relative to the working directory.}
  \item{tz}{\emph{character}. The time zone for the time stamp of the coordinates.}
  \item{stringsAsFactors}{\emph{logical}. Should character vectors be converted to factors.}
}

\details{
To avoid problems with the time zone it is advisable to set it. Otherwise the system time is used and this can result in different times on different computers. See \link[base]{timezones}.
}

\value{
Data frame as \code{\link{psyo}}.
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
Dr. Elze Ufkes
}

\seealso{
  \code{\link{dir_add_csv}}, \link[base]{timezones}
}

\examples{\donttest{\donttest{
gpx_dir <- system.file("extdata", package="psyosphere")
psyo_rounds <- dir_get_gpx(gpx_dir, tz="MET")
}}}
