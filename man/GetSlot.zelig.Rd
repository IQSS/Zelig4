\name{GetSlot.zelig}
\alias{GetSlot.zelig}
\title{Return a Value from a \code{zelig} Fitted Model}
\usage{
  \method{GetSlot}{zelig}(obj, key, ...)
}
\arguments{
  \item{obj}{a \code{zelig} object}

  \item{key}{a character-string specifying the which value
  to extract from the fitted model object}

  \item{...}{subsequent values to extract from the fitted
  model object}
}
\value{
  values of the specified keys
}
\description{
  Returns a value from the result of a model fitting
  function
}
\note{
  This function is primarily used by Zelig developers
  within \code{qi} functions
}
\author{
  Matt Owen \emph{mowen@iq.harvard.edu}
}

