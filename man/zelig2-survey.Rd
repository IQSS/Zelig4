\name{zelig2-survey}

\alias{zelig2gamma.survey}
\alias{zelig2logit.survey}
\alias{zelig2normal.survey}
\alias{zelig2poisson.survey}
\alias{zelig2probit.survey}

\title{Interface between \code{Zelig} and \code{svyglm}}

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

  zelig2logit.survey(formula, weights = NULL, ids = NULL,
    probs = NULL, strata = NULL, fpc = NULL, nest = FALSE,
    check.strata = !nest, repweights = NULL, type,
    combined.weights = FALSE, rho = NULL,
    bootstrap.average = NULL, scale = NULL, rscales = NULL,
    fpctype = "fraction", return.replicates = FALSE,
    na.action = "na.omit", start = NULL, etastart = NULL,
    mustart = NULL, offset = NULL, model1 = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, design = NULL, data)

  zelig2normal.survey(formula, weights = NULL, ids = NULL,
    probs = NULL, strata = NULL, fpc = NULL, nest = FALSE,
    check.strata = !nest, repweights = NULL, type,
    combined.weights = FALSE, rho = NULL,
    bootstrap.average = NULL, scale = NULL, rscales = NULL,
    fpctype = "fraction", return.replicates = FALSE,
    na.action = "na.omit", start = NULL, etastart = NULL,
    mustart = NULL, offset = NULL, model1 = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, design = NULL, data)

  zelig2poisson.survey(formula, weights = NULL, ids = NULL,
    probs = NULL, strata = NULL, fpc = NULL, nest = FALSE,
    check.strata = !nest, repweights = NULL, type,
    combined.weights = FALSE, rho = NULL,
    bootstrap.average = NULL, scale = NULL, rscales = NULL,
    fpctype = "fraction", return.replicates = FALSE,
    na.action = "na.omit", start = NULL, etastart = NULL,
    mustart = NULL, offset = NULL, model1 = TRUE,
    method = "glm.fit", x = FALSE, y = TRUE,
    contrasts = NULL, design = NULL, data)

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

  \item{link}{an object specifying a link function between the predictor and response variables}

  \item{data}{a \code{data.frame}}

  \item{\dots}{
    Additional parameters passed to teh ``gamma.survey'' model fitting function
  }
}
\value{
  a \code{list} used to construct parameters for the
  \code{svyglm} function
}
\description{
  Interface between \code{zelig} and \code{svyglm} for the
  \code{logit.survey}
}
\note{
  This manual file is largely incomplete, and needs a
  significant amount of filling out. This, in itself, might
  be motivation to divide this package into more models
  with more specific function.
}

