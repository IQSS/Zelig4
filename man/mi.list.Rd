\name{mi.list}
\alias{mi.list}
\title{Construct an 'mi' object from a list of data...}
\usage{mi.list(lis, by, data.labels)}
\description{Construct an 'mi' object from a list of data.frame's}
\details{This is the main constructor; all other constructors - in one fashion
or another - call this method to construct the final list of data-frames.
Care should be taken when developing code for this method, as it can
cause a myriad of additional problems.}
\value{an object of type `mi'}
\author{Matt Owen \email{mowen@iq.harvard.}}
\arguments{\item{lis}{a list of data.frames}
\item{by}{a character-string specifying a calumn of the data.frames
used in the `mi' object.}
\item{data.labels}{a vector of character-strings that specify the
name of each data.frame. This parameter is currently
ignored, and is included exclusively for future
Zelig releases}}
