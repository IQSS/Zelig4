\name{summary.glm.robust}
\alias{summary.glm.robust}
\title{Summary of Generalized Linear Model with Robust Error Estimates}
\usage{
  \method{summary}{glm.robust}(object, ...)
}
\arguments{
  \item{object}{a ``glm.robust'' fitted model}

  \item{...}{parameters to pass to the standard
  ``summary.glm'' method}
}
\value{
  a object of type ``summary.glm.robust'' and
  ``summary.glm''
}
\description{
  Returns summary of a glm model with robust error
  estimates. This only slightly differs from how the
  standard GLM's behave.
}

