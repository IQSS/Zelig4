#' Constructor for `parameters' class
#'
#'
#' @param simulations a vector or matrix containing simulated values
#' @param alpha ancillary parameters for the Zelig statistical model
#' @param fam a family object which implicitly specifies the link
#'            and link-inverse functions for the 
#' @param link the link function of the specified statistical model.
#'             The `linkinv' parameter is implicitly defined by
#'             by the `link' parameter, when `linkinv' is omitted
#' @param linkinv the inverse link function
#' @return a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
parameters <- function(simulations, alpha,
                       fam=NULL,
                       link=NULL,
                       linkinv=NULL)
{
  if (is.function(fam))
    fam <- fam()

  #
  if (!missing(fam) && isS4(fam)) {
    link <- fam@link
    linkinv <- fam@inverse
  }
  else if (!missing(fam) && inherits(fam, "family")) {
    link <- fam$linkfun
    linkinv <- fam$linkinv
  }
  else if (missing(link)) {
    #warning("no link function")
  }

  else if (missing(linkinv)) {
    #warning("no inverse link function")
    linkinv <- .NumInverse(link)
  }

  # construct object
  p <- list(coefficients = simulations,
            alpha = alpha,
            link = link,
            linkinv = linkinv
            )

  # cast, and return
  class(p) <- "parameters"
  p  
}


#' Extract ancillary parameters from
#' `parameters' objects
#'
#' @param param a `parameters' object
#' @return the ancillary parameters \emph{specified} for
#'         the statistical model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
alpha <- function(param)
  param$alpha


#' Extract simulations from `parmaeters' objects
#' 
#' @aliases simulations.parameters
#' @S3method coef parameters
#' @param a `parameters' object
#' @return simulations, specified by the Zelig model, of
#'         the ancillary parameters
#' @export 
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
coef.parameters <- function(param) {
  param$coef
}
  

simulations.parameters <- function(param)
  param$coefficients


#' Method for extracting the link function from `parameters' objects
#'
#' @param param a `parameters' object
#' @return the link function specified by the `param' function for the
#'         given Zelig model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
link <- function(param)
  param$link


#' Method for extracting the inverse link function from `parameters' objects
#'
#' @param param a `parameters' object
#' @return the inverse link function specified by the `param' function for the
#'         given Zelig model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
linkinv <- function(param) {
  if (is.null(param$linkinv))
    .NumInverse(param$link)
  else
    param$linkinv
}
