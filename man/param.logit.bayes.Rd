\name{param.logit.bayes}
\alias{param.logit.bayes}
\title{Simulate Parameters for the Bayesian Logistic Regression}
\usage{
  param.logit.bayes(obj, num = 1000, ...)
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
  This method simulates parameters for the Bayesian
  Logistic Regression (logit.bayes).
}

