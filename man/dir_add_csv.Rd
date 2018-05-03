\name{dir_add_csv}

\alias{dir_add_csv}

\title{
Bind CSV data to data frame
}

\description{
Read CSV file in directory and bind data to a data frame.
}

\usage{
dir_add_csv(
  tracks, dir, merge_by = "id", stringsAsFactors = default.stringsAsFactors()
)
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{dir}{The path to the CSV file.}
  \item{merge_by}{The column in the \emph{data_frame} and the CSV file that is used to merge the data.}
  \item{stringsAsFactors}{\emph{logical}. Should character vectors be converted to factors.}
}

\value{
\emph{Data frame}
}

\note{
  The function ignores multiple segments in the GPX file. If you want to find the gaps between the segments you could use the functions \code{\link{t_time_difference}} and \code{\link{mark_time_gaps}.}

  Further does the function sort the data by time.
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

\seealso{
  \code{\link{dir_get_gpx}}
}

\examples{
data(psyo)
csv_dir <- system.file("extdata", "ids.csv", package = "psyosphere")
psyo <- dir_add_csv(psyo, csv_dir)
}
