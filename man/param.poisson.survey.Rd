\name{param.poisson.survey}
\alias{param.poisson.survey}
\title{Param Method for the \code{poisson.survey} Zelig Model}
\usage{
  \method{param}{poisson.survey}(obj, num=1000, ...)
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
  Param Method for the \code{poisson.survey} Zelig Model
}
\note{
  This method is used internally by the \code{survey.zelig}
  package
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

