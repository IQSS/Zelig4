\name{plot.zelig}

\alias{plot.zelig}
\alias{plot}

\title{Graphing Quantities of Interest}

\description{ The \code{zelig} method for the generic \code{plot}
  command generates default plots for \code{\link{sim}} output with
  one-observation values in \code{x} and \code{x1}.  }

\usage{
\method{plot}{zelig}(x, xlab = "", user.par = FALSE, ...)
}

\arguments{
\item{x}{stored output from \code{\link{sim}}.  If the \code{x$x} or
  \code{x$x1} values stored in the object contain more than one
  observation, \code{plot.zelig} will return an error.  For linear or
  generalized linear models with more than one observation in \code{x$x}
  and optionally \code{x$x1}, you may use \code{\link{plot.ci}}.  }
\item{xlab}{a character string for the x-axis label for all graphs.}
\item{user.par}{a logical value indicating whether to use the default
    Zelig plotting parameters (\code{user.par = FALSE}) or
    user-defined parameters (\code{user.par = TRUE}), set using the
    \code{par} function prior to plotting. }
\item{\dots}{Additional parameters passed to \code{plot.default}.
  Because \code{plot.zelig} primarily produces diagnostic plots, many
  of these parameters are hard-coded for convenience and
  presentation. }  
}

\value{
Depending on the class of model selected, \code{plot.zelig} will
return an on-screen window with graphs of the various quantities of
interest.  You may save these plots using the commands described in
the Zelig manual (available at \url{http://gking.harvard.edu/zelig}).  
}

\seealso{ The full Zelig manual at
  \url{http://gking.harvard.edu/zelig} and \code{plot}, \code{lines},
  and \code{par}.  }

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{hplot}
