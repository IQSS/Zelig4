\name{bootstrap.negbinom}
\alias{bootstrap.negbinom}
\title{Bootstrap Parameters for Zelig ``negbinom'' GLM}
\usage{
  \method{bootstrap}{negbinom}(obj, ...)
}
\arguments{
  \item{obj}{a ``zelig'' object that will be used to
  produce boot-strapped parameters}

  \item{...}{extra parameters to be passed to the ``boot''
  method. These are typically ignored, but is included for
  further expansion.}
}
\value{
  a list containing information concerning link,
  link-inverses, etc.
}
\description{
  Returns bootstrapped parameter estimates for a
  negative-binomial GLM.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

