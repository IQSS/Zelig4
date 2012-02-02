\name{NextFrame}
\alias{NextFrame}
\title{Get Next \code{data.frame} from an \code{mi} Object}
\usage{
  NextFrame(obj, as.pair = FALSE)
}
\arguments{
  \item{obj}{an \code{mi} object}

  \item{as.pair}{a boolean specifying whether to return the
  \code{data.frame} as a list pairing the label and the
  data object}
}
\value{
  the next \code{data.frame} or NULL (if all
  \code{data.frames} have been generated). If
  \code{as.pair} is set to TRUE, then a list will be
  returned, specifying the \code{data.frame}'s label as
  ``label'' and the \code{data.frame} as ``data''
}
\description{
  Generate the next \code{data.frame}. This function
  removes the necessity for Zelig to depend on the
  \code{iterators} package.
}
\note{
  This method is intended for internal use by Zelig.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

