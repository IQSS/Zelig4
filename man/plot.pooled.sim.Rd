\name{plot.pooled.sim}
\alias{plot.pooled.sim}
\title{Method for plotting pooled simulations by confidence intervals}
\usage{
  \method{plot}{pooled.sim}(x, CI = 95, col = NULL, qi =
    "Expected Values", xlab = "", ..., leg.pos = NULL,
    leg.type = 1, leg.col = "gray30")
}
\arguments{
  \item{x}{A `sim' object}

  \item{CI}{A number, between 0 and 100, specifying the
  confidence interal to construct}

  \item{xlab}{Labels for the x-axis}

  \item{...}{Parameters to be passed to the `truehist'
  function which is implicitly called for numeric
  simulations}

  \item{leg.pos}{``legend type'', exact coordinates and
  sizes for legend. Overrides argment ``leg.type''}

  \item{leg.type}{``legend position'', an integer from 1 to
  4, specifying the position of the legend. 1 to 4
  correspond to ``SE'', ``SW'', ``NW'', and ``NE''
  respectively}

  \item{leg.col}{``legend color'', an valid color used for
  plotting the line colors in the legend}
}
\value{
  the current graphical parameters. This is subject to
  change in future implementations of Zelig
}
\description{
  Plot pooled simulated quantities of interest.
}
\author{
  James Honaker, adapted by Matt Owen
  \email{mowen@iq.harvard.edu}
}

