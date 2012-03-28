\name{as.bootvector}
\alias{as.bootvector}
\title{Convert Boot Object to a Vector}
\usage{
  as.bootvector(obj)
}
\arguments{
  \item{obj}{a list with two slots: ``alpha'' and ``beta''.
  Respectively, these represent bootstrap samples for
  ancillary parameters and systematic component of the
  bootstrapped GLM.}
}
\value{
  a list containing the resulting vector, as well as an
  object used to reverse-build the list (``obj'') from the
  resulting call to ``bootstrap''.
}
\description{
  Receives a list with 2 slots as its input, and returns a
  vector of the two smashed together alongwith the offsets
  used to reverse-construct the object.
}
\note{
  This method is used internally by Zelig to allow an
  intuitive, ``param''-like API for bootstrapping.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

