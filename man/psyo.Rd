\name{psyo}

\alias{psyo}

\docType{data}

\title{
Example how data should be formatted.
}

\description{
Example how data in psyosphere should be formatted.
}

\usage{data(psyo)}

\format{
  A data frame with 15 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{\emph{A character or numeric vector.} Id for each unique track.}
    \item{\code{p_id}}{\emph{A character or numeric vector}. Unique by time sorted ID for every coordinate within a track.}
    \item{\code{time}}{\emph{A POSIXct.} Date and time of the coordinate.}
    \item{\code{lon}}{\emph{A numeric vector.} Longitude of a coordinate in degree.}
    \item{\code{lat}}{\emph{A numeric vector.} Latitude of a coordinate in degree.}
  }
}

\details{
The example data contain three different tracks ("01.gpx", "02.gpx", "03.gpx") and 5 observations / coordinates for each track.

The data frame should at least contain the variables mentioned above. Additional columns can be added. Where possible the package will preserve these columns.
}

\seealso{
  \code{\link{val_psyo}}
}

\examples{\dontrun{
# Simple example ---------------------------------------------------------------

data(psyo)
print(psyo)

# Result:
#
#     id p_id                time     lon      lat
# 01.gpx    0 2016-06-19 12:37:53 4.93078 52.31003
# 01.gpx    1 2016-06-19 12:37:58 4.93038 52.30985
# 01.gpx    2 2016-06-19 12:38:08 4.92958 52.30953
# 01.gpx    3 2016-06-19 12:38:18 4.92803 52.30883
# 01.gpx    4 2016-06-19 12:38:28 4.92652 52.30800
# 02.gpx    0 2016-06-19 11:28:25 4.93580 52.31450
# 02.gpx    1 2016-06-19 11:28:38 4.93580 52.31450
# 02.gpx    2 2016-06-19 11:32:03 4.93580 52.31450
# 02.gpx    3 2016-06-19 11:32:13 4.93580 52.31450
# 02.gpx    4 2016-06-19 11:32:28 4.93580 52.31450
# 03.gpx    0 2016-06-20 10:17:08 5.00828 52.35005
# 03.gpx    1 2016-06-20 10:17:18 5.00843 52.35010
# 03.gpx    2 2016-06-20 10:17:28 5.00847 52.35028
# 03.gpx    3 2016-06-20 10:17:43 5.00847 52.35028
# 03.gpx    4 2016-06-20 10:17:53 5.00847 52.35028

# How to create a geodata data frame from scratch -------------------------------

id <- c("01.gpx", "01.gpx", "01.gpx", "01.gpx", "01.gpx",
        "02.gpx", "02.gpx", "02.gpx", "02.gpx", "02.gpx",
        "03.gpx", "03.gpx", "03.gpx", "03.gpx", "03.gpx")

p_id <- c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4)

time <- c("2016-06-19 12:37:53", "2016-06-19 12:37:58", "2016-06-19 12:38:08",
          "2016-06-19 12:38:18", "2016-06-19 12:38:28",
          "2016-06-19 11:28:25", "2016-06-19 11:28:38", "2016-06-19 11:32:03",
          "2016-06-19 11:32:13", "2016-06-19 11:32:28",
          "2016-06-20 10:17:08", "2016-06-20 10:17:18", "2016-06-20 10:17:28",
          "2016-06-20 10:17:43", "2016-06-20 10:17:53")

lon <- c(4.93078, 4.93038, 4.92958, 4.92803, 4.92652,
         4.93580, 4.93580, 4.93580, 4.93580, 4.93580,
         5.00828, 5.00843, 5.00847, 5.00847, 5.00847)

lat <- c(52.31003, 52.30985, 52.30953, 52.30883, 52.30800,
         52.31450, 52.31450, 52.31450, 52.31450, 52.31450,
         52.35005, 52.35010, 52.35028, 52.35028, 52.35028)

psyo <- data.frame(id, p_id, time, lon, lat)
psyo <- as.POSIXct(psyo$time)

remove(id, p_id, time, lon, lat)

print(psyo)
}}

\keyword{datasets}
