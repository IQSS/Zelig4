\name{param.relogit}
\alias{param.relogit}
\title{Estimate Parameters for the ``relogit'' Zelig Mdoel}
\usage{
  param.relogit(obj, num, ...)
}
\arguments{
  \item{obj}{a zelig object containing the fitted model}

  \item{num}{an integer specifying the number of
  simulations to compute}

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
\note{
  This method merely calls ``param.logit''.
}

