
\name{des_summary}

\alias{des_summary}

\title{
Creates a data frame with a summary of descriptive information for each track
}
\description{
Creates a data frame with a summary of descriptive information for each track
}

\usage{
des_summary(tracks, cweight = "auto", cgaps = "", t_id = "id", des_df = "")
}

\arguments{
  \item{tracks}{\code{\link{psyo}}. Data frame with tracks.}
  \item{cweight}{\code{character}. Column name of \code{tracks} that contains the weight for the calculation.}
  \item{cgaps}{\code{character}. Column name of \code{tracks} that marks gaps with \code{TRUE}.}
  \item{t_id}{\emph{character} or \emph{numeric}. Column name in \code{tracks} that identifies the separate tracks.}
  \item{des_df}{\code{data frame}. Function results will be merge with this data frame.}
}

\value{
Data frame
  \item{id}{id of the track}
  \item{begin_time}{begin time of the track}
  \item{end_time}{end time of the track}
  \item{duration_in_mins}{duration in minutes}
  \item{tracker_interval_in_seconds}{tracker interval in seconds}
  \item{coordinates}{number of coordinates without gaps}
  \item{coordinates_gapped}{number of coordinates with gaps}
  \item{coordinates_all}{number of all coordinates}
  \item{time_duplicates}{coordinates with the same time}
  \item{time_gap}{coordinates that are marked as gaps because they differ to much from the tracker interval}
  \item{speed_gap}{coordinates that are mark's as gaps because they have a higher speed than expected}
  \item{sum_km}{total number of kilometres}
  \item{mean_kmh}{average speed in kmh}
  \item{mean_kmh_no_stop}{average speed in kmh without coordinates with 0 speed}
  \item{movement_time_sum}{total time without speed higher than 0}
  \item{no_movement_time_sum}{total time with speed equal to 0}
  \item{move_by_no_move_ratio}{ratio between movement and no movement time}
  \item{time_good_sum}{data with good data}
  \item{time_missing_sum}{data that is missing}
  \item{time_gap_sum}{data that is excluded by gaps}
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
  \code{\link{des_duplicates}}, \code{\link{des_first}}, \code{\link{des_last}}, \code{\link{des_length}}, \code{\link{des_max}}, \code{\link{des_min}}, \code{\link{des_mean}}, \code{\link{des_sd}}, \code{\link{des_sum}}
}

\examples{\donttest{
data(psyo_rounds2)
tracks <- psyo_rounds2
descriptive <- des_summary(tracks)
}}
