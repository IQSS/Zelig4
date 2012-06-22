\name{bootstrap.gamma}
\alias{bootstrap.gamma}
\title{Bootstrap Parameters for Zelig ``gamma'' GLM}
\usage{
  \method{bootstrap}{gamma}(obj, ...)
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
  Returns bootstrapped parameter estimates for a ``gamma''
  GLM.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

