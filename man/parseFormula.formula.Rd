\name{parseFormula.Formula}
\alias{parseFormula.Formula}
\title{Parse ``Formula''-style Zelig Formulae}
\usage{
  \method{parseFormula}{Formula}(obj, data=NULL)
}
\arguments{
  \item{obj}{a list of formulae}

  \item{data}{a data frame}
}
\value{
  an object of type ``parseFormula''
}
\description{
  This method parses a ``Formula''-style Zelig formula.
  This is to support the ``Formula'' object. It seems like
  it has the right idea when it comes to expressing
  multiple responses.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

