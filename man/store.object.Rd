\name{store.object}
\alias{store.object}
\title{Store Object in Environment with a Fake Name}
\usage{
  store.object(obj, envir, name = NULL, prefix = ".")
}
\arguments{
  \item{obj}{any object}

  \item{envir}{an environment object, which will contain
  the object with the assigned name}

  \item{name}{a character-string specifying the name that
  the object will be stored as in the specified
  environment}

  \item{prefix}{a character string specifying the prefixes
  to append to names that already have matches in the
  destination environment}
}
\value{
  a character-string specifying the name of the object in
  the destination environment
}
\description{
  This function takes the value of an object and stores it
  within a specified environment. This is similar to simply
  using the \code{assign} function, but will not overwrite
  existing values in the specified environment. It
  accomplishes this by appending a prefix to the name of
  the variable until the name becomes unique.
}
\note{
  This method does not correct invalid names. That is,
  there is no test to determine whether the submitted name
  is valid.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

