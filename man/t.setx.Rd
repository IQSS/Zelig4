\name{t.setx}
\alias{t.setx}
\title{Matrix Transpose of a ``setx'' Object}
\usage{
  \method{t}{setx}(x)
}
\arguments{
  \item{x}{a `setx' object}
}
\value{
  a transposed matrix
}
\description{
  Returns a ``setx'' object as column vector. If multiple
  values for each explanatory term has been set, then
  return a NxM matrix where `N' is the number of
  explanatory terms and `M' is the number of values set for
  each term.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

