\name{plot.sim}
\alias{plot.sim}
\title{Method for plotting simulations}
\usage{
  \method{plot}{sim}(x, xlab = "", ...)
}
\arguments{
  \item{x}{a `sim' object}

  \item{...}{parameters to be passed to the `truehist'
  function which is implicitly called for numeric
  simulations}
}
\value{
  the current graphical parameters. This is subject to
  change in future implementations of Zelig
}
\description{
  Plot simulated quantities of interest.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

