\name{zelig2negbinom}
\alias{zelig2negbinom}
\title{Interface between negbinom model and Zelig
This function is exclusively for use by the `zelig' function}
\usage{
  zelig2negbinom(formula, weights = NULL, ..., data)
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
  Interface between negbinom model and Zelig This function
  is exclusively for use by the `zelig' function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

