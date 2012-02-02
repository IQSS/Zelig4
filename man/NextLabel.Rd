\name{NextLabel}
\alias{NextLabel}
\title{Get Details of Next \code{data.frame} from an \code{mi} Object}
\usage{
  NextLabel(obj)
}
\arguments{
  \item{obj}{an \code{mi} object}
}
\value{
  a character-vector specifying the next representation of
  a label or NULL (if all labels have been produced)
}
\description{
  Produce the label to be used for the next
  \code{data.frame} stored within the \code{mi} object.
  This function replaces the necessity for Zelig to depend
  on the \code{iterators} package.
}
\note{
  This method is intended for internal use by Zelig.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

