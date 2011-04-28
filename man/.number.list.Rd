\name{.number.list}
\alias{.number.list}
\title{Append Numbers to Identically Valued Strings...}
\usage{.number.list(vec)}
\description{Append Numbers to Identically Valued Strings
This function ensures that vectors of strings are uniquely named.}
\note{This function is used in tandem with '.acronym' to correctly produce
short-names for quantities of interest.}
\value{a vector of character-strings of shorter length. Duplicate hits on
short-titled names append a number to the end. E.g.: the character vector
if vec equals c('ev', 'ev', 'pr'), then the result will be:
c('ev1', 'ev2', 'pr')}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{vec}{a vector of character-string}}
