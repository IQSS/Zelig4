\name{.acronym}
\alias{.acronym}
\title{Convert a Vector of Character Strings into Acronyms...}
\usage{.acronym(str, fail=str)}
\description{Convert a Vector of Character Strings into Acronyms
This function will convert a vector of character strings into their
appropriately titled acronym forms. That is, the two Quantity of Interest
titles:
\begin{itemize}
\item "Expected Values (for X): E(Y|X)"
\item "Expected Values (for X1): E(Y|X1)"
\end{itemize}
The result will be: "ev" and "ev". That is, the acronym will not contain
information kept in paranetheses or after a colon.}
\note{This function currently includes preopositions as parts of acroynms}
\value{a vector of character-strings}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{str}{a vector of character strings to convert into acronymns}
\item{fail}{a result to produce upon failure}}
