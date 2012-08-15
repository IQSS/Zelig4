\name{plot.pooled.sim}
\alias{plot.pooled.sim}
\title{Method for plotting pooled simulations by confidence intervals}
\usage{
  \method{plot}{pooled.sim}(x, xlab = "", ...)
}
\arguments{
  \item{x}{a `sim' object}

  \item{CI}{a number, between 0 and 100, specifying the
  confidence interal to construct}

  \item{xlab}{labels for the x-axis}

  \item{...}{parameters to be passed to the `truehist'
  function which is implicitly called for numeric
  simulations}
}
\value{
  the current graphical parameters. This is subject to
  change in future implementations of Zelig
}
\description{
  Plot pooled simulated quantities of interest.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

