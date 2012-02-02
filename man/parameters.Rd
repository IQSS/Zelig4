\name{parameters}
\alias{parameters}
\title{Constructor for `parameters' class}
\usage{
  parameters(simulations, alpha, fam = NULL, link = NULL,
    linkinv = NULL)
}
\arguments{
  \item{simulations}{a vector or matrix containing
  simulated values}

  \item{alpha}{ancillary parameters for the Zelig
  statistical model}

  \item{fam}{a family object which implicitly specifies the
  link and link-inverse functions for the}

  \item{link}{the link function of the specified
  statistical model.  The `linkinv' parameter is implicitly
  defined by by the `link' parameter, when `linkinv' is
  omitted}

  \item{linkinv}{the inverse link function}
}
\value{
  a `parameters' object
}
\description{
  Constructor for `parameters' class
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

