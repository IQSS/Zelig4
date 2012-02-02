\name{model.matrix.parseFormula}
\alias{model.matrix.parseFormula}
\title{Construct Design Matrix from a Parsed, Zelig-style Formula}
\usage{
  \method{model.matrix}{parseFormula}(object, data = NULL,
    ...)
}
\arguments{
  \item{object}{a "parseFormula" object}

  \item{data}{a "data.frame"}

  \item{...}{ignored parameters}
}
\value{
  a "model.matrix" specifying information relevant to a
  statistical model
}
\description{
  This method constructs a design matrix from a Zelig-style
  formula. This matrix is commonly used in statistical
  simulation, and will likely be relevent as the relevant
  form of a \code{setx} object.
}
\note{
  This method is primarily used by the \code{setx}
  function.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

