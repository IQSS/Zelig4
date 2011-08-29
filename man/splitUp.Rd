\name{splitUp}
\alias{splitUp}
\title{Split a List into Two Lists...}
\usage{splitUp(args)}
\description{Split a List into Two Lists
This functions takes any list, and splits into two lists - one containing
the values of arguments with specifically specified values and those without
specified values.}
\note{This function is a good candidate for deprecation}
\value{a list containing two entries: the key-value paired entires (titled
wordful) and the unkeyed entried (titled wordless)}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{args}{a list}}
\examples{#list(wordful = list(x=1, y=2), wordless=list(2, "red"))}
