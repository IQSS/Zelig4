\name{param}
\alias{param}
\title{Generic Method for Simulating Ancillary/Auxillary Parameters of Zelig
  Models}
\usage{
  param(obj, num, ...)
}
\arguments{
  \item{obj}{a \code{zelig} object}

  \item{num}{an integer specifying the number of
  simulations to sample}

  \item{...}{optional parameters which will likely be
  ignored}
}
\value{
  The main purpose of the \code{param} function is to
  return a list of key-value pairs, specifuing information
  that should be shared between the \code{qi} function and
  the fitted statistical model (produced by the
  \code{zelig2} function. This list can contain the
  following entries:

  \item{\code{simulations}}{specifies a set of simulated
  parameters used to describe the statistical model's
  underlying distribution} \item{\code{alpha}}{specifies
  the fixed (non-simulated) ancillary parameters used by
  the statistical model's underlying distribution}
  \item{\code{family}}{specifies a family object used to
  implicitly define the \code{link} and \code{linkinv}
  functions. That is, this specifies the "link" and
  "inverse link" functions of generalized linear models}
  \item{\code{link}}{specifies the \code{link} function to
  be used. This parameter is largely unimportant compared
  to the "inverse link" function}
  \item{\code{linkinv}}{specifies the \code{linkinv}
  function to be used.}
}
\description{
  The \code{param} method is used by developers to specify
  simulated and fixed ancillary parameters of the Zelig
  statistical model. That is, this method is used between
  the \link{zelig2} function and the \link{qi} as a helper
  function that specifies all the necessary details needed
  to simulate quantities of interest, given the fitted
  statistical model produced by the \code{zelig2} function.
}
\note{
  The 'param' function is a method meant to be overloaded
  by Zelig Developers
}
\examples{
param.some.model <- function (obj, num, ...) {
  list(
       simulations = NULL,
       alpha = NULL,
       link = NULL,
       linkinv = NULL,
       fam = NULL
       )
}
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

