\name{summary.pooled.sim}
\alias{summary.pooled.sim}
\title{Return a Summary of a Set of Pooled Simulated Interests}
\usage{
  \method{summary}{pooled.sim}(object, ...)
}
\arguments{
  \item{object}{a ``pooled.sim'' object, containing
  information about simulated quantities of interest}

  \item{...}{Ignored parameters}
}
\value{
  a ``summary.pooled.sim'' object storing the replicated
  quantities of interest
}
\description{
  Returns the summary information from a set of pooled
  simulated interests. The object returned contains the
  slots ``labels'', a character-vector specifying the
  labels (explanatory variable titles) of the qi's,
  ``titles'', a character vector specifying the names of
  the quantities of interest, and
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

