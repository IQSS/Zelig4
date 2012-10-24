\name{zelig2probit.survey}
\alias{zelig2probit.survey}
\title{Interface between \code{zelig} and \code{svyglm} for the \code{probit.survey}}
\usage{
  zelig2probit.survey(formula, weights = NULL, ids = NULL,
    probs = NULL, strata = NULL, fpc = NULL, nest = FALSE,
    check.strata = !nest, repweights = NULL, type,
    combined.weights = FALSE, rho = NULL,
    bootstrap.average = NULL, scale = NULL, rscales = NULL,
    fpctype = "fraction", return.replicates = FALSE,
    na.action = "na.omit", start = NULL, etastart = NULL,
    mustart = NULL, offset = NULL, model1 = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, design = NULL, data)
}
\arguments{
  \item{formula}{a \code{formula}}

  \item{weights}{...}

  \item{ids}{...}

  \item{probs}{...}

  \item{strata}{...}

  \item{fpc}{...}

  \item{nest}{...}

  \item{check.strata}{...}

  \item{repweights}{...}

  \item{type}{...}

  \item{combined.weights}{...}

  \item{rho}{...}

  \item{bootstrap.average}{...}

  \item{scale}{...}

  \item{rscales}{...}

  \item{fpctype}{...}

  \item{return.replicates}{...}

  \item{na.action}{...}

  \item{start}{...}

  \item{etastart}{...}

  \item{mustart}{...}

  \item{offset}{...}

  \item{model1}{...}

  \item{method}{...}

  \item{x}{...}

  \item{y}{...}

  \item{contrasts}{...}

  \item{design}{...}

  \item{data}{a \code{data.frame}}
}
\value{
  a \code{list} used to construct parameters for the
  \code{svyglm} function
}
\description{
  Interface between \code{zelig} and \code{svyglm} for the
  \code{probit.survey}
}
\note{
  This manual file is largely incomplete, and needs a
  significant amount of filling out. This, in itself, might
  be motivation to divide this package into more models
  with more specific function.
}

