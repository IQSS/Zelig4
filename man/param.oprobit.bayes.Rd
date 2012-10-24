\name{param.oprobit.bayes}
\alias{param.oprobit.bayes}
\title{Simulate Parameters for the Bayesian Logistic Regression}
\usage{
  param.oprobit.bayes(obj, num = 1000, ...)
}
\arguments{
  \item{obj}{a zelig object}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{...}{ignored parameters}
}
\value{
  a list specifying link, link-inverse, random samples, and
  ancillary parameters
}
\description{
  Simulate Parameters for the Bayesian Logistic Regression
}
\note{
  Both the \code{param} and \code{sim} method for Bayesian
  regressions seem to ignore the \code{num} parameter. This
  method simulates parameters for the Bayesian Ordianal
  Probit Regression (oprobit.bayes).
}

