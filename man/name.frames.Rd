\name{name.frames}
\alias{name.frames}
\title{Replace Unnamed Indices with a Default Value}
\usage{
  name.frames(frames, frame.names)
}
\arguments{
  \item{frames}{q list of \code{data.frame}'s}

  \item{frame.names}{a character vecotory specifying the
  appropriate names to label each index of \code{frames} if
  no label is already set. If every \code{data.frame}
  already has a label, then the \code{frame.names}
  parameter is essentially ignored}
}
\value{
  a character-string specifying the appropriate names for
  the \code{data.frame}'s
}
\description{
  Replace Unnamed Indices with a Default Value
}
\note{
  This method is used by Zelig to organize collections of
  data.frame's for use with multiple imputation. That is,
  this function correctly labels lists of data.frame's
  submitted to the code{zelig} function.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

