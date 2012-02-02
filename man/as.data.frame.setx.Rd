\name{as.data.frame.setx}
\alias{as.data.frame.setx}
\title{Coerce a \code{setx} Object into a \code{data.frame}}
\usage{
  \method{as.data.frame}{setx}(x, row.names=NULL,
    optional=FALSE, ...)
}
\arguments{
  \item{x}{a \code{setx} object}

  \item{row.names}{ignored parameter}

  \item{optional}{ignored parameter}

  \item{...}{ignored parameters}
}
\value{
  the \code{setx} object interpretted as a
  \code{data.frame}. The column-names of the resulting
  \code{data.frame} are specified by the names of the
  \code{setx} object. The row-names are typically
  unlabeled.
}
\description{
  Coerce a \code{setx} Object into a \code{data.frame}
}
\note{
  In subsequent versions of Zelig, this version is expected
  to undergo minor modifications.
}

