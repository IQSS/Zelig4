\name{bootstrap.normal}
\alias{bootstrap.normal}
\title{Bootstrap Parameters for Zelig ``normal'' GLM}
\usage{
  \method{bootstrap}{default}(obj, num, ...)
}
\arguments{
  \item{obj}{a ``zelig'' object that will be used to
  produce boot-strapped parameters}

  \item{num}{an integer specifying the number of
  simulations to produce}

  \item{...}{extra parameters to be passed to the ``boot''
  method. These are typically ignored, but is included for
  further expansion.}
}
\value{
  a list containing information concerning link,
  link-inverses, etc.
}
\description{
  Returns bootstrapped parameter estimates for a Gaussian
  GLM.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

