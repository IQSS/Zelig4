\name{zwhat}
\alias{zwhat}
\title{Utility function for counterfactual predictions using Zelig models}
\usage{
  zwhat(model, cfs)
}
\arguments{
\item{model}{Output of a Zelig model.}
\item{cfs}{Dataset of counterfactual examples.} 
}
\value{
  the counterfactual dataset where the DV is the mean expected value and 
  generated variable \code{inHull} is a dummy for each observation.
}
\description{
  The \code{zwhat} utility allows users to easily make predictions from
  counterfactual datasets and check if they are in the convex hull of the data.
}
\author{
  Vito D'Orazio
}


