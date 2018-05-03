\name{about demos}
\alias{about_demos}

\title{
Explanation how the demo files can be accessed
}

\description{
The package contains some demonstrations how the different functions can be used. The demonstrations are experiments that are used to develop 'psyosphere', thus it is real experimental data.

Be aware the that it can take two hours ore more to complete some of the demos. Therefore, the demos frequently will save the progress. You can access the demo files directly to restore your progress.

You can find more information about the experiments on [analyse-gps.com](https://analyse-gps.com/experiments/ut-smuggle-experiment/).
}

\details{

  \enumerate{

    \item{\bold{Move demo file}. The demo file will download a zip file of about 2MB that unpacks into about 200MB. You can move the demo files to an appropriate location. In the example section is explained how you can find the demo files.}

    \item{\bold{Set working directory}. For the demo files to work, the working directory must be set to the directory that contains the demo. You can see in the example section how you can do that.
  }

    \item{\bold{Step by step execution}. The demos contain sometimes hundreds of thousands of coordinates. Therefore, it is advisable to calculate the demos step by step and to compress the coordinates first with \link{average_coordinates}.}

  }

}

\section{Credit}{
  If you use 'psyosphere' for commercial use or research, please support us by include one off the following references:
  \itemize{
    \item{\bold{Creative Commons:} "" by B. Ziepert, E. G. Ufkes & P. W. de Vries from analyse-gps.com / CC-BY-SA-4.0}
    \item{\bold{APA}: de Vries, P. W., et al. (2016). "De psychologie van bewegingen GPS-technologie voor de analyse van natuurlijk gedrag." Tijdschrift voor Human Factors 2: 11-15.}
  }
}

\author{
  Benjamin Ziepert. Please send feedback to: \email{feedback-psyosphere@analyse-gps.com}.
}

\examples{\donttest{
# Get a list of the demo's
demo(package = "psyosphere")

# Find the location of a demo file
system.file("demo", "smuggler1.R", package = "psyosphere")

# Run demo
demo("smuggler1", package = "psyosphere")

# How to restore progress ------------------------------------------------------

# Set directory (see top of the file)

demo_dir <- tempdir()
dir <- paste0(demo_dir,"/leadership1/")

# Clean environment and load data (see top of each section)
rm(list=setdiff(ls(), "dir"))
load(paste0(dir,"rdata/01.RData"))
}}
