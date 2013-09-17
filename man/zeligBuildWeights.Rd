\name{zeligBuildWeights}
\alias{zeligBuildWeights}
\title{Developer Utility Function for Dealing with Observation Weighting}
\usage{
  zeligBuildWeights(weights=NULL, repweights=NULL, zeros="zeros", rebuild=FALSE, allowweights=TRUE, allowrepweights=TRUE, data=NULL) 
}
\arguments{
\item{weights}{A set of non-negative value weights.  Overrides repweights 
   if defined.}
\item{repweights}{A set of whole number (non-negative integer) weights.  
   useful if weights are just for making copies or deleting certain 
   or frequency weights.}
\item{zeros}{An option on how to deal with zero valued user supplied weights.
   Default of "zero" allows zero weights, "epsilon" changes zeroes to 1e-08,
   "remove" removes those observations from the dataset.}
\item{rebuild}{An option to allow specified repweights to reconfigure the 
   rows of the dataset to rebuild a corresponding dataset where every row is
   of weight 1.  Useful if analysis model does not accept weights.}  
\item{allowweights}{Defines if weights are allowed in model.}
\item{allowrepweights}{Defines if repweights are allowed in model.  Overridden if
   \code{useweights=TRUE}.}
\item{data}{Dataset, required if weights are defined by variable name, or if
     dataset is to be reconfigured (by \code{rebuild} or \code{zeros} options)}
}
\value{
\item{weights}{A vector of weights of the structure defined by the
   developer and required by the analysis model.  Or NULL if certain checks 
   are failed.}
\item{data}{A reconfigured dataset, if modified.}
}
\description{
  The \code{zeligBuildWeights} utility allows developers
  building models or modules for Zelig, to easily define
  what types of weights can be set by the user and passed
  to estimation function.  In some cases it can reconfigure
  the dataset by duplication to functionally invoke discrete
  weighting by replication of observations, when the estimator
  itself can not utilize weights.
}
\author{
  James Honaker \email{jhonaker@iq.harvard.edu}
}
\seealso{
  The full Zelig developer manual may be accessed online at
  \url{http://gking.harvard.edu/zelig}
}
\keyword{weights}

