\name{about analysing tips}

\alias{about_analysing_tips}

\title{
Guide how to analyse GPS data
}

\description{
Guide how to analyse GPS data
}

\details{

  The following guide explains the steps to analyse GPS data.

  \enumerate{

    \item{\bold{Save data as \code{\link{psyo}} data frame}. You should create a psyo data frame by reading GPX files with \link{dir_get_gpx} and add additional information for each participant with \link{dir_add_csv}.}

    \item{\bold{Remove not needed data}. By removing data, you can greatly improve calculation speed. (1) You can remove data by specifying a begin and end time (See example section below). (2) You could create a polygon of the area you want to select with \link{mark_inside_polygon} and then select only the data you want. To determine a good polygon, you can draw the tracks with \link{plot_tracks}. (3) You can select data between a start and finish polygon with \link{select_between_polygons}.}

    \item{\bold{Create a test sample}. If you have a lot of data calculations can take hours. Especially if you use the functions \link{distance_psyo} and \link{distance_peers}. To speed things up you can first create a test sample with \link{average_coordinates} and \link{select_test_sample}. In most cases, it is better to use \link{average_coordinates}. When everything works, you can run your script with the original coordinates.}

    \item{\bold{clean-up the data}. Before the clean-up calculate the descriptive summary with \link{des_summary} so you know which data is removed. After that you can clean-up the data with the following steps. (1) You should average coordinates that have the same time stamp with \link{average_duplicates}. You can mark gaps with (2) \link{mark_speed_gaps} and (3) \link{mark_time_gaps}.}

    \item{\bold{Plot tracks}. You can plot the tracks with \link{plot_tracks} to check how the tracks look like, if the right data is removed, if the gaps are marked correctly, ...}

    \item{\bold{Calculate your data}. You now can calculate different measures. It is important to do this after removing not needed data and the clean-up. Otherwise unwanted data could be included into the calculations. If you already calculated a measure you can calculate it again and the old column will be overwritten.}

    \item{\bold{Create summary for each participant / tracker}. With \link{des_summary} and other functions with the prefix "des_" you can calculate a summary for each participant.}

  }

  You can see these steps implemented with the demo smuggler1. See also \link{about_demos}.

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
	\code{\link{about_common_mistakes}}, \code{\link{about_demos}}
}

\examples{
# Remove data before begin and after end ---------------------------------------
data(psyo_rounds2)
tracks <- psyo_rounds2
begin <- as.POSIXct("2015-09-03 14:00:00")
end <- as.POSIXct("2015-09-03 14:20:00")
tracks <- tracks[ tracks[,"time"] > begin & tracks[,"time"] < end ,]
}
