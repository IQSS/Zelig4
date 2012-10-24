\name{zelig2probit.bayes}
\alias{zelig2probit.bayes}
\title{Interface between the Zelig Model probit.bayes and
the Pre-existing Model-fitting Method}
\usage{
  zelig2probit.bayes(formula, burnin = 1000, mcmc = 10000,
    verbose = 0, ..., data)
}
\arguments{
  \item{formula}{a formula}

  \item{burnin}{a parameter corresponding to the 'burnin'
  paramater for the MCMCprobit function}

  \item{mcmc}{a parameter corresponding to the 'mcmc'
  paramater for the MCMCprobit function}

  \item{verbose}{a parameter corresponding to the 'verbose'
  paramater for the MCMCprobit function}

  \item{...}{additonal parameters}

  \item{data}{a data.frame}
}
\value{
  a list specifying '.function'
}
\description{
  Interface between the Zelig Model probit.bayes and the
  Pre-existing Model-fitting Method
}

