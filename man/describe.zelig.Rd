\name{describe.zelig}
\alias{describe.zelig}
\title{Get Description Object Used to Cite this Zelig Model}
\usage{
  \method{describe}{zelig}(object, ...)
}
\arguments{
  \item{object}{a 'zelig' object}

  \item{...}{ignored parameters}
}
\value{
  a 'description' object used internally to produce
  citation text
}
\description{
  Get Description Object Used to Cite this Zelig Model
}
\note{
  This function should be reevaluated in design, since
  'description' objects are exclusively used internally. In
  particular, this method would be more useful to users as
  a 'cite' method.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

