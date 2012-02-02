\name{labels.mi}
\alias{labels.mi}
\title{Get Labels from \code{mi} Object}
\usage{
  \method{labels}{mi}(object, ...)
}
\arguments{
  \item{object}{an \code{mi} object}

  \item{...}{ignored parameters}
}
\value{
  a character-string specifying all the labels
}
\description{
  Find a suitable set of labels for titling each individual
  \code{mi} object.
}
\note{
  This function is primarily used internally by Zelig to
  keep track of the \code{data.frame}'s produced by
  \code{mi} objects.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

