\name{zeligBuildSpline}
\alias{zeligBuildSpline}
\title{Developer Utility Function for Adding Simple Cubic Splines to Models}
\usage{
  zeligBuildSpline(formula, k, data) 
}
\arguments{
\item{formula}{A string or formula object, with a variable to spline denoted
	as \code{s(.)}}
\item{k}{A number of knot points to set equidistant across the range of the 
	variable to spline.}
\item{data}{A dataset}
}
\value{
\item{formula}{A modified formula, including the names of all the basis generated
	basis function variables, and removing the \code{s(.)} notation.}
\item{data}{A reconfigured dataset, if modified, with spline basis function 
	variables added.}
}
\description{
  The \code{zeligBuildSpline} allows developers to include simple cubic smoothing
  splines of one dimension in models, by constructing \code{k} knot points 
  equidistantly spaced across some variable, and adding those variables to the 
  formula expression as a basis function for the spline.
}
\author{
  James Honaker \email{jhonaker@iq.harvard.edu}
}
\seealso{
  The full Zelig developer manual may be accessed online at
  \url{http://gking.harvard.edu/zelig}
}
\keyword{weights}

