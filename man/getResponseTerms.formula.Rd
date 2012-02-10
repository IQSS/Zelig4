\name{getResponseTerms.Formula}
\alias{getResponseTerms.Formula}
\title{Get Response Terms from a ``Formula'' Object}
\usage{
  \method{getResponseTerms}{formula}(x, ...,
    single.only=FALSE, duplicates=TRUE)
}
\arguments{
  \item{x}{a formula}

  \item{...}{ignored parameters}

  \item{single.only}{a logical specifying whether 'cbind'
  or 'list' keywords are allowed}

  \item{duplicates}{a logical specifying whether the
  returned character-vector will only return duplicates.}
}
\value{
  a character-vector specifying the response terms of the
  formula
}
\description{
  This method gets the response terms from a ``Formula''
  Object
}
\author{
  Matt Owen
}

