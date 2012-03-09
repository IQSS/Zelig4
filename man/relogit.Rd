\name{relogit}
\alias{relogit}
\title{Fit a rare-event logistic model in Zelig}
\usage{
  relogit(formula, data = sys.parent(), tau = NULL,
    bias.correct = TRUE, case.control = "prior", ...)
}
\arguments{
  \item{formula}{a formula object}

  \item{data}{...}

  \item{tau}{...}

  \item{bias.correct}{...}

  \item{case.control}{...}

  \item{...}{???}
}
\value{
  a ``Relogit'' ``glm'' object
}
\description{
  Fits a rare-event (``Relogit'') model.
}

