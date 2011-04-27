\name{reset}
\alias{reset}
\title{Generic method for resetting an iterator...}
\usage{reset(obj, ...)}
\description{Generic method for resetting an iterator
Resets the incrementor of an iterator}
\note{This method is not exported; it is used internally by Zelig, but is not
registered for use in the Global namespace or search path. It is unclear
whether the  'reset' method is a desirable feature for other packages}
\value{the original object, except with its internatl
iterator pointing to the first element of its list}
\arguments{\item{obj}{the iterator-like object to be reset}
\item{...}{a list of parameters of other parameters}}
