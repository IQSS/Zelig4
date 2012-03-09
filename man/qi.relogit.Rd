\name{qi.relogit}
\alias{qi.relogit}
\title{Simulate Quantities of interest for the Zelig ``relogit'' Model}
\usage{
  qi.relogit(obj, x = NULL, x1 = NULL, y = NULL,
    num = 1000, param = NULL)
}
\arguments{
  \item{obj}{a zelig object, containing the fitted
  ``relogit'' model}

  \item{x}{a ``setx'' object}

  \item{x1}{a ``setx'' object}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{param}{a ``parameter'' obejct containing
  information about the link, inverse-link, and simulated
  parameters}
}
\value{
  a param
}
\description{
  Simulate Quantities of interest for the Zelig ``relogit''
  Model
}

