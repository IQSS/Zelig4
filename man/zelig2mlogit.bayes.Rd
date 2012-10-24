\name{zelig2mlogit.bayes}
\alias{zelig2mlogit.bayes}
\title{Interface between the Zelig Model mlogit.bayes and
the Pre-existing Model-fitting Method}
\usage{
  zelig2mlogit.bayes(formula, burnin = 1000, mcmc = 10000,
    verbose = 0, ..., data)
}
\arguments{
  \item{formula}{a formula}

  \item{burnin}{a parameter corresponding to the 'burnin'
  paramater for the MCMCmnl function}

  \item{mcmc}{a parameter corresponding to the 'mcmc'
  paramater for the MCMCmnl function}

  \item{verbose}{a parameter corresponding to the 'verbose'
  paramater for the MCMCmnl function}

  \item{...}{additonal parameters}

  \item{data}{a data.frame}
}
\value{
  a list specifying '.function'
}
\description{
  Interface between the Zelig Model mlogit.bayes and the
  Pre-existing Model-fitting Method
}

