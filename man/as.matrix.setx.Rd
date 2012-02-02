\name{as.matrix.setx}
\alias{as.matrix.setx}
\title{Convert a 'setx' Object to a Matrix}
\usage{
  as.matrix.setx(x, ...)
}
\arguments{
  \item{x}{a setx object}

  \item{...}{ignored parameters}
}
\value{
  a matrix containing columns and rows corrseponding to the
  explanatory variables specified in the call to the 'setx'
  function
}
\description{
  The setx object is, in its most basic form, a list of
  column names and values specified for each of these
  column names. This function simply converts the key-value
  pairs of column-name and specified value into a matrix.
}
\note{
  This method allows basic matrix arithmetic operations on
  data objects, which mirror values stored within setx
  objects. In many scenarios, simulations require
  matrix-multiplication, etc. to be performed on a
  data-set. This function faciliates that need.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

