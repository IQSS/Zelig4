\name{qi.relogit2}
\alias{qi.relogit2}
\title{simulate quantities of interest for the zelig ``relogit'' model}
\usage{
  qi.relogit2(object, simpar, x, x1 = NULL, y = NULL)
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
  simulate quantities of interest for the zelig ``relogit''
  model
}

