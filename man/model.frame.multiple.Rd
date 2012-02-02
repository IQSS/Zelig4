\name{model.frame.multiple}
\alias{model.frame.multiple}
\title{Create Model Frame from \code{multiple} Object}
\usage{
  \method{model.frame}{multiple}(formula,data,eqn=NULL,...)
}
\arguments{
  \item{formula}{an object of both type \code{formula} and
  \code{multiple}}

  \item{data}{a \code{data.frame}}

  \item{eqn}{the number of equations in the formula}

  \item{...}{ignored parameters}
}
\value{
  a \code{model.frame} object
}
\description{
  This method creates a \code{model.frame} from a
  \code{multiple} object. This method will be deprecated as
  the development of Zelig 4 progresses.
}
\author{
  Kosuke Imai, Olivia Lau, Gary King and Ferdinand Alimadhi
}

