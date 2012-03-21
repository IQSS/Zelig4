\name{zelig2logit}
\alias{zelig2logit}
\title{Interface between logit model and Zelig}
\usage{
  zelig2logit(formula, weights = NULL, robust = F, ...,
    data)
}
\arguments{
  \item{formula}{a formula}

  \item{weights}{a numeric vector}

  \item{...}{ignored parameters}

  \item{data}{a data.frame}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  This function is exclusively for use by the `zelig'
  function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

