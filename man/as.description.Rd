\name{as.description}
\alias{as.description}
\title{Generic Method for Casting 'description' Objects}
\usage{
  as.description(descr, ...)
}
\arguments{
  \item{descr}{an object to cast an object of type
  'description'}

  \item{...}{parameters which are reserved for future Zelig
  revisions}
}
\value{
  an object of type 'description'
}
\description{
  Convert the result of a call to the 'describe' method
  into an object parseble by Zelig. Currently conversions
  only exist for lists and description objects.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
\seealso{
  as.description.description as.description.list
}

