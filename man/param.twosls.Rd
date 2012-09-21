\name{param.twosls}
\alias{param.twosls}
\title{param method for the `twosls' Zelig model}
\usage{
  \method{param}{twosls}(obj, num, ...)
}
\arguments{
  \item{obj}{a `zelig' object}

  \item{num}{an integer specifying the number of
  simulations to sample}

  \item{...}{ignored parameters}
}
\value{
  a list to be cast as a `parameters' object
}
\description{
  Return parameter estimates for the ``twosls'' GLM in
  Zelig.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

