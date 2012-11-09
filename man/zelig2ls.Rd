\name{zelig2ls}
\alias{zelig2ls}
\title{Interface between ls model and Zelig
This function is exclusively for use by the `zelig' function}
\usage{
  zelig2ls(formula, ..., data, weights = NULL)
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
  Interface between ls model and Zelig This function is
  exclusively for use by the `zelig' function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

