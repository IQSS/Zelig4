\name{robust.glm.hook}
\alias{robust.glm.hook}
\title{Hook for ``glm'' Models in Zelig}
\usage{
  robust.glm.hook(obj, zcall, call, robust = FALSE, ...)
}
\arguments{
  \item{obj}{a zelig object}

  \item{zcall}{the original call to the zelig model}

  \item{call}{the call that will be evaluated for the}

  \item{robust}{a logical specifying whether or not to use
  robust error estimates}

  \item{...}{ignored parameters}
}
\value{
  the fitted model object
}
\description{
  Adds support for robust error-estimates in the Zelig
  ``glm'' models.
}

