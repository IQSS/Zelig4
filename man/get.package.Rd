\name{get.package}
\alias{get.package}
\title{Find the Zelig package that a particular model belong to}
\usage{
  get.package(model, quiet = TRUE, ...)
}
\arguments{
  \item{model}{a character-string specifying a Zelig model}

  \item{quiet}{a logical indicating whether to display
  messages and warnings}

  \item{...}{ignored parameters}
}
\value{
  NA or a character-string specifying the name of the
  package which contains a specific model
}
\description{
  This method is used to help transition Zelig v3.5 users
  to Zelig v4
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

