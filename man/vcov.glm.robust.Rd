\name{vcov.glm.robust}
\alias{vcov.glm.robust}
\title{Return Variance-Covariance Matrix}
\usage{
  \method{vcov}{glm.robust}(object, ...)
}
\arguments{
  \item{object}{an object of type ``glm.robust''}

  \item{...}{parameters passed forward to
  ``summary.glm.robust''}
}
\value{
  a NxN matrix, where N is the number of parameters in the
  fitted model.
}
\description{
  Returns the variance-covariance matrix produced for this
  model.
}

