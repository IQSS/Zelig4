\name{zelig2}

\alias{zelig2factor.bayes}
\alias{zelig2gamma}
\alias{zelig2gamma.gee}
\alias{zelig2gamma.survey}
\alias{zelig2logit}
\alias{zelig2logit.bayes}
\alias{zelig2logit.survey}
\alias{zelig2lognorm}
\alias{zelig2ls}
\alias{zelig2mlogit.bayes}
\alias{zelig2negbinom}
\alias{zelig2normal}
\alias{zelig2normal.bayes}
\alias{zelig2normal.gee}
\alias{zelig2normal.survey}
\alias{zelig2oprobit.bayes}
\alias{zelig2poisson}
\alias{zelig2poisson.bayes}
\alias{zelig2poisson.gee}
\alias{zelig2poisson.survey}
\alias{zelig2probit}
\alias{zelig2probit.bayes}
\alias{zelig2probit.gee}
\alias{zelig2probit.survey}
\alias{zelig2relogit}
\alias{zelig2tobit}
\alias{zelig2twosls}

\title{Interface between `zelig' and Model-Fitting Function}
\note{This is used internally by Zelig, and is written by contributing developers.}
\usage{
  zelig2gamma(formula, ..., data)
}
\arguments{
  \item{formula}{a formula}

  \item{...}{ignored parameters}

  \item{data}{a data.frame}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  Interface between gamma model and Zelig This function is
  exclusively for use by the `zelig' function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
