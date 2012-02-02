\name{linkinv}
\alias{linkinv}
\title{Method for extracting the inverse link function from 'parameters' objects}
\usage{
  linkinv(param)
}
\arguments{
  \item{param}{a 'parameters' object}
}
\value{
  the inverse link function specified by the 'param'
  function for the given Zelig model
}
\description{
  Returns the inverse link function of a ``parameters''
  object. If the model's developer did not specify one (but
  did specify a link function) this function returns a
  numerical approximation of the link function.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

