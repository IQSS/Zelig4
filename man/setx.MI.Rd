\name{setx.MI}
\alias{setx.MI}
\title{Set Explanatory Variables for Multiply Imputed Data-sets
This function simply calls setx.default once for every fitted model
within the 'zelig.MI' object}
\usage{
  \method{setx}{MI}(obj, ..., data=NULL)
}
\arguments{
  \item{obj}{a 'zelig' object}

  \item{...}{user-defined values of specific variables for
  overwriting the default values set by the function
  \code{fn}}

  \item{data}{a new data-frame}
}
\value{
  a 'setx.mi' object used for computing Quantities of
  Interest by the 'sim' method
}
\description{
  Set Explanatory Variables for Multiply Imputed Data-sets
  This function simply calls setx.default once for every
  fitted model within the 'zelig.MI' object
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
\seealso{
  \link{setx}
}

