\name{qi.relogit2}
\alias{qi.relogit2}
\title{simulate quantities of interest for the zelig ``relogit'' model}
\usage{
  \method{qi}{relogit2}(obj, x=NULL, x1=NULL, y=NULL,
    num=1000, param=NULL)
}
\arguments{
  \item{obj}{a zelig object, containing the fitted
  ``relogit'' model}

  \item{x}{a ``setx'' object}

  \item{x1}{a ``setx'' object}

  \item{y}{this parameter is reserved for simulating
  average treatment effects, though this feature is
  currentlysupported by only a handful of models}

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
  ...
}

