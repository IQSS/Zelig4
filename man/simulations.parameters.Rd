\name{simulations.parameters}
\alias{simulations.parameters}
\title{Return Simulations of Parameter Coefficients}
\usage{
  \method{simulations}{parameters}(object, ...)
}
\arguments{
  \item{object}{a 'parameters' object}

  \item{\dots}{ignored}
}
\value{
  simulations, specified by the Zelig model, of the
  ancillary parameters
}
\description{
  Returns simulated parameters of coefficients for use in
  statistical simulation. The values are set by the
  model-fitting function and the developer of the qi.<model
  name> method.
}
\note{
  This function does not differ at all from coef.default
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

