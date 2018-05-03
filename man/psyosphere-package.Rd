\name{psyosphere}
\alias{psyosphere-package}

\title{psyosphere details}

\description{
'psyosphere' can be used to analyse location data (latitude, longitude, elevation). Based on spherical trigonometry, variables such as speed, bearing, and distances can be calculated from moment to moment, depending on the sampling frequency of the equipment used, and independent of scale. Additionally, the package can plot tracks, coordinates, and shapes on maps, and sub-tracks can be selected with point-in-polygon or other techniques. The package is optimized to support behavioural science experiments with multiple tracks. It can detect and clean up errors in the data, and resulting data can be exported to be analysed in statistical software or geographic information systems (GIS).
}

\docType{package}

\details{

  'psyosphere' uses geodata data frames to store tracks. For the format, you can read \code{\link{psyo}}.

  The package handles latitude and longitude and mostly ignores elevation.

  For the first steps please read \link{about_analysing_tips}

}

\section{Credit}{
  If you use 'psyosphere' for commercial use or research, please support us by include one off the following references:
  \itemize{
    \item{\bold{MIT license:} "psyosphere" by B. Ziepert, E. G. Ufkes & P. W. de Vries from https://CRAN.R-project.org/package=psyosphere}
    \item{\bold{APA}: Ziepert, B., Ufkes, E., & de Vries, P. W. (2018). psyosphere: Analyse GPS Data. Retrieved from https://CRAN.R-project.org/package=psyosphere}
    \item{\bold{APA}: Vries, P., Ziepert, B., & Ufkes, E. (2016). "De psychologie van bewegingen GPS-technologie voor de analyse van natuurlijk gedrag." Tijdschrift voor Human Factors 2: 11-15.}
  }
}

\section{Credits}{
  For 'psyosphere' we made use of the following software:

  \itemize{
    \item{"R" by The R foundation from r-project.org / GNU-2}
    \item{"RStudio" by RStudio from rstudio.com / AGPL v3}

    \item{"ggmap" by D. Kahle & H. Wickham from cran.rstudio.com / GPL-2}
    \item{"rgdal" by R. Bivand, T. Keitt, B. Rowlingson, E. Pebesma, M. Sumner, R. Hijmans & E. Rouault from cran.rstudio.com / GPL-2}
    \item{"plyr" by H. Wickham from cran.rstudio.com / MIT}
    \item{"geosphere" by R. Hijmans, E. Williams & C. Vennes from cran.rstudio.com / GPL-3}
    \item{"ggplot2" by H. Wickham, W. Chang & RStudio from cran.rstudio.com / GPL-2}
    \item{"sp" by E. Pebesma, R. Bivand, B. Rowlingson, V. Gomez-Rubio, R. Hijmans, M. Sumner, D. MacQueen, J. Lemon & J. O'Brien from cran.rstudio.com / GPL-2}
    \item{"lubridate" by G. Grolemund, V. Spinu, H. Wickham, I. Lyttle, I. Constigan, J. Law, D. Mitarotonda, J. Larmarange, J. Boiser & C. Lee from cran.rstudio.com / GPL-2}
    \item{"RgoogleMaps" by M. Loecher from cran.rstudio.com / GPL}
    \item{"SDMTools" by J. VanDerWal, L. Falconi, S. Januchowski, L. Shoo & C. Storlie from cran.rstudio.com / GPL-3}

  }
}

\author{
  Benjamin Ziepert. \email{feedback-psyosphere@analyse-gps.com}.

  Dr. Elze G. Ufkes \email{elze.ufkes@utwente.nl}.

  Dr. Ir. Peter W. de Vries \email{p.w.devries@utwente.nl}.

}
