\name{zelig2-bayes}

\alias{zelig2factor.bayes}
\alias{zelig2logit.bayes}
\alias{zelig2mlogit.bayes}
\alias{zelig2normal.bayes}
\alias{zelig2oprobit.bayes}
\alias{zelig2poisson.bayes}
\alias{zelig2probit.bayes}

\title{Zelig Bridge Functions to Bayesian Models}

\usage{
  zelig2factor.bayes(formula, factors = 2, burnin = 1000, mcmc = 20000, verbose = 0, ..., data)
  zelig2logit.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
  zelig2mlogit.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
  zelig2normal.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
  zelig2oprobit.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
  zelig2poisson.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
  zelig2probit.bayes(formula, burnin = 1000, mcmc = 10000, verbose = 0, ..., data)
}

\arguments{
  \item{formula}{a formula}

  \item{...}{additonal parameters}

  \item{data}{a data.frame}

  \item{factors}{a list of factors}

  \item{burnin}{a parameter corresponding to the 'burnin'
  paramater for the MCMCprobit function}

  \item{mcmc}{a parameter corresponding to the 'mcmc'
  paramater for the MCMCprobit function}

  \item{verbose}{a parameter corresponding to the 'verbose'
  paramater for the MCMCprobit function}
}

\value{
  a list specifying '.function'
}

\description{
  Interface between Zelig and the bayesian models.
}
