\name{param.normal.survey}
\alias{param.normal.survey}
\title{Param Method for the \code{normal.survey} Zelig Model}
\usage{
  \method{param}{normal.survey}(obj, num=1000, ...)
}
\arguments{
  \item{obj}{a \code{zelig} object}

  \item{num}{an integer specifying the number of
  simulations to sample}

  \item{...}{ignored parameters}
}
\value{
  a list to be cast as a \code{parameters} object
}
\description{
  Param Method for the \code{normal.survey} Zelig Model
}
\note{
  This method is used internally by the \code{survey.zelig}
  package
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

