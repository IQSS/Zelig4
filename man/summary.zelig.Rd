\name{summary.zelig}

\alias{summary.zelig}
\alias{summary}

\title{Summary of Simulated Quantities of Interest}

\description{Summarizes the object of class \code{\link{zelig}} (output
  from \code{\link{sim}}) which contains simulated quantities of
  interst.} 

\usage{
\method{summary}{zelig}(object, subset = NULL, CI = 95, stats = c("mean", "sd"), \dots)
}

\arguments{
  \item{object}{output object from \code{\link{sim}} (of class
    \code{"zelig"}).}
  \item{subset}{takes one of three values:
    \itemize{
    \item{NULL}{(default) for more than one observation, summarizes all the
      observations at once for each quantity of interest.}
    \item{a numeric vector}{indicates which observations to summarize,
      and summarizes each one independently.}
    \item{all}{summarizes all the observations independently.}
    }
  }
  \item{stats}{summary statistics to be calculated.}
  \item{CI}{a confidence interval to be calculated.}
  \item{\ldots}{further arguments passed to or from other methods.}
}

\value{
  \item{sim}{number of simulations, i.e., posterior draws.}
  \item{x}{values of explanatory variables used for simulation.}
  \item{x1}{values of explanatory variables used for simulation of first
    differences etc.}
  \item{qi.stats}{summary of quantities of interst.  Use
    \code{\link{names}} to view the model-specific items available in
    \code{qi.stats}.}
}

\seealso{
  \code{\link{zelig}}, \code{\link{setx}}, \code{\link{sim}},
  and \code{\link{names}}, and the full Zelig manual at
  \url{http://gking.harvard.edu/zelig}.  }

\author{
  Kosuke Imai <\email{kimai@princeton.edu}>; Gary King
  <\email{king@harvard.edu}>; Olivia Lau <\email{olau@fas.harvard.edu}>
}

\keyword{file}





