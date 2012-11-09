\name{zelig2normal}
\alias{zelig2normal}
\title{Interface between normal model and Zelig
This function is exclusively for use by the `zelig' function}
\usage{
  zelig2normal(formula, weights = NULL, ..., data)
}
\arguments{
  \item{formula}{a formula}

  \item{weights}{a numeric vector}

  \item{...}{ignored parameters}

  \item{data}{a data.frame}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  Interface between normal model and Zelig This function is
  exclusively for use by the `zelig' function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

