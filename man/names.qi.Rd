\name{names.qi}
\alias{names.qi}
\title{The Names of a 'qi' Object...}
\usage{names.qi(x)}
\description{The Names of a 'qi' Object
Function to get the names of a 'qi' object. This function does not entirely
parallel the functionality of traditional 'names' methods; this is because
the \code{$} operator has been overloaded to support a unique style of value
extraction. For technical details, please see the source code.}
\note{No method exists to set the names of a 'qi' object, once it is 
constructed. This will be a feature added later.}
\value{a character-vector containing the names of the Quantities of
Interest}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{x}{a 'qi' object}}
