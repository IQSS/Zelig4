\name{zelig2exp}
\alias{zelig2exp}
\title{Interface between the Zelig Model exp and
the Pre-existing Model-fitting Method}
\usage{
  zelig2exp(formula, ..., robust = FALSE, cluster = NULL,
    data)
}
\arguments{
  \item{formula}{a formula}

  \item{...}{additonal parameters}

  \item{robust}{a boolean specifying whether to use robust
  error estimates}

  \item{cluster}{a vector describing the clustering of the
  data}

  \item{data}{a data.frame}
}
\value{
  a list specifying '.function'
}
\description{
  Interface between the Zelig Model exp and the
  Pre-existing Model-fitting Method
}

