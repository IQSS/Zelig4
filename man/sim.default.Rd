\name{sim.default}
\alias{sim.default}
\title{Method for Simulating Quantities of Interest wrom 'zelig' Objects}
\usage{
  sim.default(obj, x = NULL, x1 = NULL, y = NULL,
    num = 1000, bootstrap = FALSE, bootfn = NULL,
    cond.data = NULL, ...)
}
\arguments{
  \item{obj}{a 'zelig' object}

  \item{x}{a 'setx' object}

  \item{x1}{a secondary 'setx' object used to perform
  particular computations of quantities of interest}

  \item{y}{a parameter reserved for the computation of
  particular quantities of interest (average treatment
  effects). Few models currently support this parameter}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{bootstrap}{ignored}

  \item{bootfn}{ignored}

  \item{cond.data}{ignored}

  \item{...}{special parameters which are reserved for
  future versions of Zelig}
}
\value{
  a 'sim' object storing the replicated quantities of
  interest
}
\description{
  Method for Simulating Quantities of Interest wrom 'zelig'
  Objects
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

