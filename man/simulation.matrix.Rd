\name{simulation.matrix}
\alias{simulation.matrix}
\title{Get Simulations as a Matrix}
\usage{
  simulation.matrix(obj, which = NULL, ...)
}
\arguments{
  \item{obj}{an object, typically a ``sim'' or
  ``pooled.sim'' object.}

  \item{which}{a character-vector specifying the
  \emph{titles} of quantities of interest to extract}

  \item{...}{additional parameters}
}
\value{
  a simulation matrix
}
\description{
  Returns a MxN matrix where N is the number of simulations
  and M is the number of predicted values. Additionally, a
  ``labels'' attribute is attached that produces a
  human-readable identifier for each column.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

