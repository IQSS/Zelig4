#z' Generic method for casting various objects as a
#' `parameters' object
#' 
#' @param params the object to be casted
#' @param ... 
#' @return an object of type `parameters'
#' @export
#' @author Matt Owen \email{mowen@@ig.harvard.edu}
as.parameters <- function(params, ...)
  UseMethod("as.parameters")


#' list -> parameters
#' @param params a list object
#' @param num an integer specifying the number of simulations
#'        to be taken
#' @param ... ignored parameters
#' @return an object of type `parameters'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.parameters.list <- function(params, num=NULL, ...) {
  #
  coefficients <- if ("simulations" %in% names(params))
    params$simulations
  else if (num < length(params))
    params[1:num]
  else
    params[[1]]

  # 
  alpha <- if ("alpha" %in% names(params))
    params$alpha
  else if (num < length(params))
    tail(params, -num)


  # link function
  link <- if (!is.null(params$link))
    params$link

  # link-inverse function
  linkinv <- if (!is.null(params$linkinv))
    params$linkinv

  # family object, has both a link and link-inverse
  fam <- params$fam

  # return
  parameters(coefficients, alpha, fam=fam, link=link, linkinv=linkinv)
}


#' parameters -> parameters
#'
#' @param paramss a parameters object
#' @param ... ignored parameters
#' @return the same parameter object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.parameters.parameters <- function(params, ...)
  params


#' ??? -> parameters
#' @param params any non-supported data-type
#' @return the object passed in
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.parameters.default <- function(params, num=NULL, ...) {
  if (!missing(num)) {
    alpha <- if (num < nrow(params))
      tail(params, -num)

    #
    parameters(simulations=head(params, num), alpha=alpha)
  }
  
  else
    parameters(simulations=params, alpha=NULL)
}
