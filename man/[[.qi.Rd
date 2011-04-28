\name{[[.qi}
\alias{[[.qi}
\title{Extraction Operator for Quantities of Interest...}
\usage{[[.qi(self, key)}
\description{Extraction Operator for Quantities of Interest
This function is exclusively used internally by Zelig, and behaves in a very
fishy manner. \code{qi} objects maintain an internal list of indices which
are used to find the appropriate slot which holds a particular quantity of
interest.
When a \code{qi} object is defined, all the quantities of interest are
converted into acronyms, so that elements of the \code{qi} object can be
stored without a lengthy name containing spaces (since most qi's are
human-readable). As a result, these objects contain an \code{.index}
attribute which pairs every quantity of interest with its acronym. This
index is then used to extract (using the \code{$} operator) the appropriate
element of the list.
In short, it pairs the key "Expected Value" with the slot \code{ev}. This
allows that the following will always be true (in the mentioned example):
\code{qi$ev == qi[["Expected Value"]]}}
\note{When possible, \code{qi} objects should be handled with iterators
rather than list-style extraction operators.}
\value{if the quantity of interest exists, that entry. Otherwise,
\code{NULL}}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{self}{the \code{qi} object}
\item{key}{a character-string specifying the title of the quantity of
interest to extract.}}
