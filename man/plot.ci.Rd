\name{plot.ci}
\alias{plot.ci}
\title{Method for plotting pooled simulations by confidence intervals}
\usage{
  \method{plot}{ci}(x, qi="ev", var=NULL, ...,
    legcol="gray20", col=NULL, leg=1, legpos=NULL)
}
\arguments{
  \item{x}{A `sim' object}

  \item{qi}{a character-string specifying the quantity of
  interest to plot}

  \item{var}{The variable to be used on the x-axis. Default
  is the variable across all the chosen values with
  smallest nonzero variance}

  \item{...}{Parameters to be passed to the `truehist'
  function which is implicitly called for numeric
  simulations}

  \item{legcol}{``legend color'', an valid color used for
  plotting the line colors in the legend}

  \item{col}{a valid vector of colors of at least length 3
  to use to color the confidence intervals}

  \item{leg}{``legend position'', an integer from 1 to 4,
  specifying the position of the legend. 1 to 4 correspond
  to ``SE'', ``SW'', ``NW'', and ``NE'' respectively}

  \item{legpos}{``legend type'', exact coordinates and
  sizes for legend. Overrides argment ``leg.type''}
}
\value{
  the current graphical parameters. This is subject to
  change in future implementations of Zelig
}
\description{
  Plot confidence intervals of pooled simulated values.
}
\author{
  James Honaker, adapted by Matt Owen
  \email{mowen@iq.harvard.edu}
}

