\name{zelig2probit}
\alias{zelig2probit}
\title{Interface between probit model and Zelig
This function is exclusively for use by the `zelig' function}
\usage{
  zelig2probit(formula, weights = NULL, ..., data)
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
  Interface between probit model and Zelig This function is
  exclusively for use by the `zelig' function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

