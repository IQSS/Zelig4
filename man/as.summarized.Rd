\name{as.summarized}
\alias{as.summarized}
\title{Generic Method for Casting Objectst as 'summarized' Objects}
\usage{
  as.summarized(x, ...)
}
\arguments{
  \item{x}{an object}

  \item{...}{unspecified parameters}
}
\value{
  a 'summarized.qi' object
}
\description{
  This function is particularly for use by the 'summarize'
  method, which summarizes the simulations taken from the
  'qi' method. The generic function 'summary' when applied
  to a Zelig Simulation implicitly uses this function.
}
\note{
  This is made available on the Global namespace as a
  matter of potential future compliancy.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

