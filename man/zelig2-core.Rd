\name{zelig2-core}

\alias{zelig2exp}
\alias{zelig2gamma}
\alias{zelig2logit}
\alias{zelig2lognorm}
\alias{zelig2ls}
\alias{zelig2negbinom}
\alias{zelig2normal}
\alias{zelig2poisson}
\alias{zelig2probit}
\alias{zelig2relogit}
\alias{zelig2tobit}
\alias{zelig2twosls}

\title{
  Zelig to Basic GLM Fitting Functions
}

\usage{
  zelig2exp(formula, ..., robust = FALSE, cluster = NULL, data)
  zelig2gamma(formula, ..., data)
  zelig2logit(formula, weights = NULL, robust = F, ..., data)
  zelig2lognorm(formula, ..., robust = FALSE, cluster = NULL, data)
  zelig2ls(formula, ..., data, weights = NULL)
  zelig2negbinom(formula, weights = NULL, ..., data)
  zelig2normal(formula, weights = NULL, ..., data)
  zelig2poisson(formula, weights = NULL, ..., data)
  zelig2probit(formula, weights = NULL, ..., data)
  zelig2relogit(formula, ..., tau = NULL, bias.correct = NULL, case.control = NULL, data)
  zelig2tobit(formula, ..., below = 0, above = Inf, robust = FALSE, cluster = NULL, data)
  zelig2twosls(formula, ..., data)
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

  \item{tau}{...}

  \item{bias.correct}{...}

  \item{case.control}{...}

  \item{below}{a numeric or infinite specifying a lower
  boundary for censored responses}

  \item{above}{a numeric or infinite specifying an upper
  boundary for censored responses}
}

\value{
  a list used by Zelig to call the model-fitting function
}

\description{
  Interface between Zelig and Basic GLM Fitting Functions
}
