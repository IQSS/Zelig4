\name{mi}

\alias{mi}

\title{Bundle multiply imputed data sets as a list}

\description{The code \code{mi} bundles multiply imputed data sets as a
  list for further analysis.}

\usage{
  mi(...)
}

\arguments{ 
  \item{...}{multiply imputed data sets, separated by commas. The
    arguments can be tagged by \code{name=data} where \code{name} is the
    element named used for the data set \code{data}.}
}

\value{The list containing each multiply imputed data set as an
  element. The class name is \code{mi}. The list can be inputted into
  \code{zelig} for statistical analysis with multiply imputed data
  sets. See \code{zelig} for details.
}

\examples{
  data(immi1, immi2, immi3, immi4, immi5)
  mi(immi1, immi2, immi3, immi4, immi5)
}

\seealso{ The full Zelig manual is available at
  \url{http://gking.harvard.edu/zelig}.
}

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{methods}
