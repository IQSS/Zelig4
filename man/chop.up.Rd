\name{chop.up}
\alias{chop.up}
\title{Split a Parameter List into Two Lists...}
\usage{chop.up(...)}
\description{Split a Parameter List into Two Lists}
\note{This is essentially a wrapper function for \link{split.up}. This
was used internally by Zelig, but now is primarily syntactical sugar for
less important features.}
\value{a list containing two entries: the key-value paired entires (titled
wordful) and the unkeyed entried (titled wordless)}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\seealso{split.up}
\arguments{\item{...}{a mix of parameters of any time}}
\examples{chop.up(x=1, 2, "red", y=2)
#list(wordful = list(x=1, y=2), wordless=list(2, "red"))}
