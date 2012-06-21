\name{name.object}
\alias{name.object}
\title{Name Elements of an Object}
\usage{
  name.object(obj, names)
}
\arguments{
  \item{obj}{a vector or matrix}

  \item{names}{a character-vector specifying names}
}
\value{
  the original object, with a "colnames" or "names" equal
  to the parameter "names". If "names" is larger than
  "obj", the "names" parameter is truncated appropriately.
  If it is smaller, then the latter part of "obj" is
  replaced with a numbered generic column name
}
\description{
  Returns an object
}
\note{
  This method is used internally by Zelig to name the
  columns and elements of matrices and vectors for
  simulations and bootstrapped parameters.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

