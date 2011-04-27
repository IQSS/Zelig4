\name{nextElem.mi}
\alias{nextElem.mi}
\title{Extract the Next Data-frame from an 'mi' Object...}
\usage{\method{nextElem}{mi}(obj, \dots, keys.only=F, as.pair=F)}
\description{Extract the Next Data-frame from an 'mi' Object
Produces the next data-frame from the iterator list. If
the 'keys.only' parameter  is set to TRUE (defaults FALSE), then
exclusively the human-readable label is listed. If the 'as.pair'
parametet is set to TRUE (defaults FALSE), then a list containing
two entries ("key" and "value") are displayed. "key" contains the
label of the data-frame as a character string, and "value" contains
the actual subsetted data-frame.}
\value{the next data.frame, title of the data.frame, or key-value
pair of title and data.frame}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{obj}{an object of type 'mi'}
\item{...}{ignored parameters}
\item{keys.only}{a boolean specifying whether the return value
should exclusively be the title of the next data.frame or
the key-value pair of the title and the actual data.frame}
\item{as.pair}{a boolean specifying whether the next element should
be return as a key-value pair}}
