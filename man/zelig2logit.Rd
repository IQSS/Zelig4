\name{zelig2logit}
\alias{zelig2logit}
\title{Interface between logit model and Zelig}
\usage{
}
\arguments{
  \item{formula}{a formula}

  \item{weights}{a numeric vector}

  \item{robust}{a boolean (logical) specifying whether
  robust error estimates should be used}

  \item{...}{ignored parameters}

  \item{data}{a data.frame}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  This function is exclusively for use by the `zelig'
  function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

