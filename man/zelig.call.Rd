\name{zelig.call}
\alias{zelig.call}
\title{Create Function Call}
\usage{
  zelig.call(Call, zelig2, remove = NULL)
}
\arguments{
  \item{Call}{a \code{call} object, typically specifying
  the original function call to \code{zelig}}

  \item{zelig2}{the return-value of the \code{zelig2}
  method}

  \item{remove}{a list of character vectors specifying
  which parameters to ignore from the original call to
  \code{zelig}}
}
\value{
  a function call used to fit the statistical model
}
\description{
  Create Function Call
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

