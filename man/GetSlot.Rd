\name{GetSlot}
\alias{GetSlot}
\title{Generic method for extracting variables from both
S3 and S4 fitted model object}
\usage{
  GetSlot(obj, key, ...)
}
\arguments{
  \item{obj}{an object of type `zelig'}

  \item{key}{a character-string specifying the name of the
  variable to extract}

  \item{...}{typically ignored parameters}
}
\value{
  the value of that extracted object or NULL
}
\description{
  Generic method for extracting variables from both S3 and
  S4 fitted model object
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

