\name{mi}
\alias{mi}
\title{Bundle Multiply Imputed Data Sets as a List...}
\usage{mi(..., by)}
\description{Bundle Multiply Imputed Data Sets as a List}
\details{The mi constructor bundles several data-frames with identical 
columns into a single object. This allows for several analyses
to be executed sequentially.}
\value{an object of type 'mi'}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\seealso{The full Zelig manual is available at
\url{http://gking.harvard.edu/zelig}}
\arguments{\item{...}{an object or set objects to cast as an 'mi' object}
\item{by}{a character-string specifying a column name
in a data-frame to subset}}
