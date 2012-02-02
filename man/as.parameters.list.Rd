\name{as.parameters.list}
\alias{as.parameters.list}
\title{list -> parameters}
\usage{
  as.parameters.list(params, num = NULL, ...)
}
\arguments{
  \item{params}{a list object}

  \item{num}{an integer specifying the number of
  simulations to be taken}

  \item{...}{ignored parameters}
}
\value{
  an object of type `parameters'
}
\description{
  The list may contain: 'link', 'linkinv', 'family',
  'alpha', and 'simulations' keys.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
\seealso{
  as.parameters
}

