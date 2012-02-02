\name{parseFormula}
\alias{parseFormula}
\title{Parse Zelig-style Formulae}
\usage{
  parseFormula(obj, data = NULL)
}
\arguments{
  \item{obj}{a list or formula}

  \item{data}{the data set associated with the formula
  object}
}
\value{
  an object of type "parseFormula". This object has slots
  specifying:
}
\description{
  Zelig uses three distinct types of formulae. This method
  is a re-design of the Zelig function
  \code{parse.formula}.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

