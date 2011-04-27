\name{qi.summarize}
\alias{qi.summarize}
\title{Constructor for QI Summarized Class...}
\usage{qi.summarize(title, x, ...)}
\description{Constructor for QI Summarized Class
This class takes an arbitrary number of the _same_ type of 
quantities of interest labels them, then
merges them into one simple printable block. In particular,
this class determines which print function to use based on the
the type and size od data to be passed to the print function.}
\value{the list of QI's (invisibly)}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{title}{a character-string specifying the title of the QI}
\item{x}{a list of summarized quantities of interest}
\item{...}{additional quantities of interest (the parameter that
titles these will be used as the name of the data.frame}}
