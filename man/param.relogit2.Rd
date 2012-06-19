\name{param.relogit2}
\alias{param.relogit2}
\title{Estimate Parameters for the ``relogit'' Zelig Mdoel}
\usage{
  param.relogit2(obj, num, x, ...)
}
\arguments{
  \item{obj}{a zelig object containing the fitted model}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{x}{ideally we should be able to remove this
  parameter}

  \item{...}{unspecified parameters}
}
\value{
  a list specifying important parameters for the
  ``relogit'' model
}
\description{
  Returns estimates on parameters, as well as, specifying
  link and inverse-link functions.
}

