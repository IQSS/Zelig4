\name{find.match}
\alias{find.match}
\title{Find a Partial or Exact Match from a Vector of Strings
Searches a vector of character-string, and returns the best match.}
\usage{
  find.match(needle, haystack, fail = NA)
}
\arguments{
  \item{needle}{a character-string to search for in the}

  \item{haystack}{a vector of character-strings}

  \item{fail}{the value to return in case no match is
  found. Defaults to NA}
}
\value{
  the best-matched string or NA
}
\description{
  Find a Partial or Exact Match from a Vector of Strings
  Searches a vector of character-string, and returns the
  best match.
}
\details{
  ``find.match'' attempts to use several common matching
  functions in an order that sequentially prefers less
  strict matching, until a suitable match is found. If none
  is found, then return the value of the ``fail'' parameter
  (defaults to NA). The functions used for matching are:
  ``match'', ``charmatch'', and finally ``grep''.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

