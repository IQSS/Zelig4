\name{list.depth}
\alias{list.depth}
\title{Count the Depth of a List Object...}
\usage{list.depth(obj, list.only=TRUE)}
\description{Count the Depth of a List Object}
\details{This function recursively computes the depth of a list object. That is, it
determines how many layers or levels exist within the object.}
\note{This function is used internally by Zelig.}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{obj}{a vector or list object}
\item{list.only}{a logical specifying whether non-list objects that have a 
list structure will be treated as valid recursable object}}
