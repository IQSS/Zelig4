\name{qi.normal.survey}
\alias{qi.normal.survey}
\title{Simulate Quantities of Interest for \code{normal.survey} Model}
\usage{
  \method{qi}{normal.survey}(obj, x, x1=NULL, y=NULL,
    num=1000, param=NULL)
}
\arguments{
  \item{obj}{zelig object}

  \item{x}{setx object}

  \item{x1}{setx object}

  \item{y}{ATT variable}

  \item{num}{implicitly called by sim - number of
  simulations to run}

  \item{param}{param object contains: link, link-inverse,
  simulations, ancillary parameters}
}
\value{
  a list containing simulated quantities of interest
}
\description{
  Simulate Quantities of Interest for \code{normal.survey}
  Model
}
\note{
  This function is paraphrased from Zelig v3.4.0-1
}

