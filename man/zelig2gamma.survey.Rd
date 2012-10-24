\name{zelig2gamma.survey}
\alias{zelig2gamma.survey}
\title{Interface between \code{zelig} and \code{svyglm} for the \code{gamma.survey}}
\usage{
  zelig2gamma.survey(formula, weights = NULL, ids = NULL,
    probs = NULL, strata = NULL, fpc = NULL, nest = FALSE,
    check.strata = !nest, repweights = NULL, type,
    combined.weights = FALSE, rho = NULL,
    bootstrap.average = NULL, scale = NULL, rscales = NULL,
    fpctype = "fraction", return.replicates = FALSE,
    na.action = "na.omit", start = NULL, etastart = NULL,
    mustart = NULL, offset = NULL, model1 = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, design = NULL, link = "inverse",
    data, ...)
}
\arguments{
  \item{formula}{a \code{formula}}

  \item{weights}{NULL}

  \item{ids}{NULL}

  \item{probs}{NULL}

  \item{strata}{NULL}

  \item{fpc}{NULL}

  \item{nest}{FALSE}

  \item{check.strata}{!nest}

  \item{repweights}{NULL}

  \item{type}{... ...}

  \item{combined.weights}{FALSE}

  \item{rho}{NULL}

  \item{bootstrap.average}{NULL}

  \item{scale}{NULL}

  \item{rscales}{NULL}

  \item{fpctype}{"fraction"}

  \item{return.replicates}{FALSE}

  \item{na.action}{"na.omit"}

  \item{start}{NULL}

  \item{etastart}{NULL}

  \item{mustart}{NULL}

  \item{offset}{NULL}

  \item{model1}{TRUE}

  \item{method}{"glm.fit"}

  \item{x}{FALSE}

  \item{y}{TRUE}

  \item{contrasts}{NULL}

  \item{design}{NULL}

  \item{link}{"inverse"}

  \item{data}{a \code{data.frame}}

  \item{...}{ignored parameters}
}
\value{
  a \code{list} used to construct parameters for the
  \code{svyglm} function
}
\description{
  Interface between \code{zelig} and \code{svyglm} for the
  \code{gamma.survey}
}
\note{
  This manual file is largely incomplete, and needs a
  significant amount of filling out. This, in itself, might
  be motivation to divide this package into more models
  with more specific function.
}

