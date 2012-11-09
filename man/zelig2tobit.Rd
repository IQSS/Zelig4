\name{zelig2tobit}
\alias{zelig2tobit}
\title{Interface between the Zelig Model tobit and
the Pre-existing Model-fitting Method}
\usage{
  zelig2tobit(formula, ..., below = 0, above = Inf,
    robust = FALSE, cluster = NULL, data)
}
\arguments{
  \item{formula}{a formula}

  \item{...}{additonal parameters}

  \item{below}{a numeric or infinite specifying a lower
  boundary for censored responses}

  \item{above}{a numeric or infinite specifying an upper
  boundary for censored responses}

  \item{robust}{a boolean specifying whether to produce
  robust error estimates}

  \item{cluster}{...}

  \item{data}{a data.frame}
}
\value{
  a list specifying '.function'
}
\description{
  Interface between the Zelig Model tobit and the
  Pre-existing Model-fitting Method
}

