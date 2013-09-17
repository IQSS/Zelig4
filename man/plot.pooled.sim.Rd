\name{plot.pooled.sim}
\alias{plot.pooled.sim}
\title{Method for plotting pooled simulations by confidence intervals}
\usage{
  \method{plot}{pooled.sim}(x, qi="ev", var=NULL, ..., main = NULL, sub = NULL, 
  xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, legcol="gray20", col=NULL, leg=1,
   legpos=NULL, ci=c(80,95,99.9))
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

  \item{main}{A character-string, specifying the main title of the plot}

  \item{sub}{A character-string, specifying the sub-title of the plot}

  \item{xlab}{A character-string, specifying the label for the x-axis}

  \item{ylab}{A character-string, specifying the label for the y-axis}

  \item{xlim}{A vector of length 2, specifying the left-most and right-most values for the plot}

  \item{ylim}{A vector of length 2, specifying the bottom-most and top-most values for the plot}

  \item{legcol}{``legend color'', an valid color used for
  plotting the line colors in the legend}

  \item{col}{a valid vector of colors of at least length 3
  to use to color the confidence intervals}

  \item{leg}{``legend position'', an integer from 1 to 4,
  specifying the position of the legend. 1 to 4 correspond
  to ``SE'', ``SW'', ``NW'', and ``NE'' respectively}

  \item{legpos}{``legend type'', exact coordinates and
  sizes for legend. Overrides argment ``leg.type''}

  \item{ci}{A numeric triple, specifying the three levels 
  to plot confidence intervals for, scaled 0 to 100}
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

