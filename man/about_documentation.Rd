\name{about documentation}

\alias{about documentation}

\title{psyosphere documentation guideline}

\description{
  Guideline for creating 'psyosphere' documentation.
}

\details{

  \bold{File names}

  \enumerate{
    \item {The file names of documentation about 'psyosphere' in general begins with "about ". Exception is the package documentation file.}
    \item {The file names cannot begin with "aa" or "ab" to prevent that they are listed before the "about" files.}.
    \item {The file names for functions documentations are identical to the function names. Exception are private functions. See also \href{about_functions.html}{about functions}}.
    \item {The file name for data always begin with "psyo".}
  }

\bold{Files}

  For each documentation 2 files will be created.
  \enumerate{
    \item {A documentation file. A file that contains the documentation for the function and has the same file name as the function file. See also: \href{about_functions.html}{about functions}.}
    \item {A documentation example file. A file that contains the example code for the documentation. This file has the same file name as the function file with the prefix "man_" and is stored in the directory "code_examples/man".}
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
