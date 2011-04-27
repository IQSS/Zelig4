\name{zframe}
\alias{zframe}
\title{Construct a Subsetted Data-Frame...}
\usage{zframe(dataf, labels)}
\description{Construct a Subsetted Data-Frame
This mysteriously named function is used internally by Zelig to process
subsetted data-frames with the 'mi' function. In particular, it is primarily
used by mi's 'nextElem' method.}
\note{This function is exclusively used internally by Zelig.}
\value{a data.frame with an overloaded operator}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{dataf}{a data.frame}
\item{labels}{a vector of character-strings (currently ignored)}}
