\name{setx.default}
\alias{setx.default}
\title{Set explanatory variables}
\usage{
  setx.default(obj, fn = NULL, data = NULL, cond = FALSE,
    ...)
}
\arguments{
  \item{obj}{a 'zelig' object}

  \item{fn}{a list of key-value pairs specifying which
  function apply to columns of the keys data-types}

  \item{data}{a data.frame}

  \item{cond}{ignored}

  \item{...}{parameters specifying what to explicitly set
  each column as. This is used to produce counterfactuals}
}
\value{
  a 'setx' object
}
\description{
  Set explanatory variables
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}, Kosuke Imai, and
  Olivia Lau
}

