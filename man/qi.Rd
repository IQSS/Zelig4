\name{qi}

\alias{qi}
\alias{qi.exp.Rd}
\alias{qi.logit.Rd}
\alias{qi.negbinom.Rd}
\alias{qi.normal.survey.Rd}
\alias{qi.poisson.survey.Rd}
\alias{qi.relogit.Rd}
\alias{qi.gamma.Rd}
\alias{qi.ls.Rd}
\alias{qi.normal.Rd}
\alias{qi.poisson.Rd}
\alias{qi.probit.Rd}
\alias{qi.relogit2.Rd}
\alias{qi.tobit.Rd}

\title{Generic Method for Computing Quantities of Interest}
\usage{
  qi(obj, x = NULL, x1 = NULL, y = NULL, num, param = NULL)
}
\arguments{
  \item{obj}{a \code{zelig} object}

  \item{x}{a \code{setx} object or NULL}

  \item{x1}{an optional \code{setx} object}

  \item{y}{this parameter is reserved for simulating
  average treatment effects, though this feature is
  currentlysupported by only a handful of models}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{param}{a parameters object}
}
\value{
  a list of key-value pairs specifying pairing titles of
  quantities of interest with their simulations
}
\description{
  The \code{qi} function is used by developers to simulated
  quantities of interest. This method, as a result, is the
  most significant method of any Zelig statistical model.
}
\note{
  Run \code{example(qi)} to see a trivial version of
}
\examples{
qi.some.model <- function(obj, x=NULL, x1=NULL, y=NULL, param=NULL) {
  list(
       "Expected Values: E(Y|X)" = NA,
       "Predicted Values: Y|X"   = NA
       )
}
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

