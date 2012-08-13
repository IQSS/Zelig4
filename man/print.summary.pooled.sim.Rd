\name{print.summary.pooled.sim}
\alias{print.summary.pooled.sim}
\title{Print a Summary of a Set of Pooled Simulated Interests}
\usage{
  \method{print}{summary.pooled.sim}(x, ...)
}
\arguments{
  \item{x}{a ``summary.pooled.sim'' object, containing
  summarized information about simulated quantities of
  interest}

  \item{...}{Optional parameters that will be passed onward
  to ``print.matrix'' (the matrix printing function)}
}
\value{
  a ``summary.pooled.sim'' object storing the quantities of
  interest
}
\description{
  Prints the summary information from a set of pooled
  simulated interests. This method assumes that quantities
  of interest are kept in a data type which can be used
  with ``rbind''.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

