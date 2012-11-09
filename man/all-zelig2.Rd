\name{all-zelig2}

\alias{zelig2exp}
\alias{zelig2lognorm}
\alias{zelig2factor.bayes}
\alias{zelig2gamma}
\alias{zelig2gamma.gee}

\title{Interface between the Zelig Model exp and
the Pre-existing Model-fitting Method}
\usage{
  zelig2exp(formula, ..., robust = FALSE, cluster = NULL,
    data)
  zelig2logit(formula, weights = NULL, robust = F, ...,
    data)
  zelig2factor.bayes(formula, factors = 2, burnin = 1000,
    mcmc = 20000, verbose = 0, ..., data)
  zelig2gamma(formula, ..., data)
  zelig2gamma.gee(formula, id, robust, ..., R,
    corstr = "independence", data)
}
\arguments{
  \item{formula}{a formula}

  \item{...}{additonal parameters}

  \item{weights}{a numeric vector}

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

