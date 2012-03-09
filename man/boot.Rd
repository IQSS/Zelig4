\name{boot}
\alias{boot}
\title{Generic Method for ``boot''}
\usage{
  boot(obj, num, ...)
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
  This mehtod is intended to be overried by statistical
  models that would like to support statistical
  bootstrapping.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

