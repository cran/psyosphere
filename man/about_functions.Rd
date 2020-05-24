\name{about functions}

\alias{about functions}

\title{psyosphere functions guideline}

\description{
  Guideline for creating 'psyosphere' functions.
}

\details{

  \bold{Function patterns}

  The different function categories follow different structures. The functions are alphabetically grouped by a prefix. The prefixes are:
  \describe{
    \item{apply}{meta tasks for data frames}
    \item{average}{average location of multiple coordinates to one coordinate}
    \item{des}{descriptive about the tracks}
    \item{dir}{getting data from directories}
    \item{distance}{calculate distance to something else for each coordinate}
    \item{mark}{\code{logical} lists}
    \item{plot}{create Google map plots}
    \item{select}{select coordinates within tracks}
    \item{t}{calculations per coordinate.}
    \item{val}{validate variables}
  }

  \bold{t group}

  The calculation information of the t group will be stored at the arriving coordinate. For example, the bearing from point 1 to point 2 will be stored with point 2 and the first point 1 of a track has a \code{NA}. Storing with point 1 or point 2 has both advantages and disadvantages. The data is stored with point 2 because of the compatibility with \code{\link{select_gaps}} and \code{\link{select_without_gaps}} When all coordinates with gaps get removed the data of the t group like speed, bearing, etc. gets also removed and for instance an average without the gaps can be calculated.

  In the example section, you can find the basic structure of the t group. All groups follow a similar basic structure.

  \bold{Function guidelines}

  The following list is a guide how a function should look like.

  Function format:
  \enumerate{
    \item {Function name is it short.}
    \item {Function name is it alphabetical logical sorted. For instance, start stored variables with "data" or directory operation with "dir".}
    \item {Function name cannot begin with "aa" or "ab" to prevent that they are listed before the "about" documentation files.}.
    \item {Check each input variable. With \code{\link{val_var}} or \code{\link{val_psyo}}}
    \item {Every function can handle the \link{psyo} format.}
    \item {No longer than 30 lines.}
    \item {Childe function are private. Functions that only support a main function and don't need to be accessed by the package user end with "_private" and are stored in the same file as the main function. Private functions can be accessed with \code{"psysophere:::"}}
  }

  File format:
  \enumerate{
    \item {Each function has its own file. Exception are private functions that end on "_private" and support the main function.}
    \item {The file name is identical with the function name}
  }

  \bold{Files}

  For each function 4 different files will be created.
  \enumerate{
    \item {A function file. A file that contains the functions itself.}
    \item {A documentation file. A file that contains the documentation for the function and has the same file name as the function file. See also: \link{about documentation}.}
    \item {A documentation example file. A file that contains the example code for the documentation. This file has the same file name as the function file with the prefix "man_" and is stored in the directory "code_examples/man".}
    \item {A test file. A file that contains the test code for the package test. This file has the same file name as the function file with the prefix "test_" and is stored in the directory "tests/testthat".}
  }

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

\examples{\dontrun{
# ------------------------------------------------------------------------------
# Example calculation R file ---------------------------------------------------
# ------------------------------------------------------------------------------

t_distance <- function(
  tracks, bind = TRUE, drop = TRUE, cname = "distances_in_m", t_id = "id"
) {

  # Check variables
  e <- val_psyo(tracks, 0, 0, 0, 2, 2); if (e != "") {stop(e)}
  e <- val_var(bind, "logical"); if (e != "") {stop(e)}
  e <- val_var(drop, "logical"); if (e != "") {stop(e)}
  e <- val_var(cname, "character"); if (e != "") {stop(e)}
  e <- val_cname(tracks, t_id); if (e != "") {stop(e)}

  # Add bearings per track
  result <- apply_tracks(
    tracks,
    "distance_exec_private(eval_track)",
    t_id = t_id
  )

  # Reformat result
  result <- data.frame(result)
  result <- plyr::rename(result, c("result" = cname))

  # Return result
  result <- bind_drop_private(tracks, result, bind, drop)
  return(result)

}

distance_exec_private <- function(tracks) {

  # Get lat and lon from next observation
  current <- subset(tracks, select = c("lon","lat"))
  previous <- apply_shift(
    tracks, "-1", FALSE, c("lon","lat"), t_id = ""
  )

  # Get distances
  distances_in_m <- geosphere::distHaversine(previous, current)

  return(distances_in_m)

}

# ------------------------------------------------------------------------------
# Template for test file -------------------------------------------------------
# ------------------------------------------------------------------------------

# Print title
cat("\nTesting <function_name>()\n")

# Get data
data("psyo_rounds2")
tracks <- psyo_rounds2

# Calculations


# Check results
# if (NROW(________) != ________) { stop("Wrong number of observations") }
# if (NCOL(________) != ________) { stop("Wrong number of variables") }
# val_psyo(________)
# test_sum <- sum(____)
# if (round(test_sum,3) != round(____,3)) {stop("Wrong test_sum")}
}}
