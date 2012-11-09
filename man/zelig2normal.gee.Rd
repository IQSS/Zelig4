\name{zelig2normal.gee}
\alias{zelig2normal.gee}
\title{Interface between the Zelig Model normal.gee and
the Pre-existing Model-fitting Method}
\usage{
  zelig2normal.gee(formula, id, robust, ..., R,
    corstr = "independence", data)
}
\arguments{
  \item{formula}{a formula}

  \item{id}{a character-string specifying the column of the
  data-set to use for clustering}

  \item{robust}{a logical specifying whether to robustly or
  naively compute the covariance matrix. This parameter is
  ignore in the \code{zelig2} method, and instead used in
  the \code{robust.hook} function, which executes after the
  call to the \code{gee} function}

  \item{...}{ignored parameters}

  \item{R}{a square-matrix specifying the correlation}

  \item{corstr}{a character-string specifying the
  correlation structure}

  \item{data}{a data.frame}
}
\value{
  a list specifying the call to the external model
}
\description{
  Interface between the Zelig Model normal.gee and the
  Pre-existing Model-fitting Method
}

