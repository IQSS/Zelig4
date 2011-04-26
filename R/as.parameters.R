#' Generic Method for Converting Objects into 'parameters'
#'
#' Converts list-style objects into Parameter lists primarily used by the 'qi'
#' methods. These list-style objects may contain keys specifying: 'link' (the 
#' link function of a statistical model), 'linkinv' (the inverse-link
#'function), 'family' (a object of 'family' class used to specify the model's
#' classification), 'alpha' (a vector of ancillary parameters, and 'simulations'
#' (a vector of simulated draws from the model's underlying distribution.
#'
#' @note Only three scenarios may exist - converting 'parameters' to
#'   'parameters', 'list' to 'parameters', and vectors to 'parameters'. The
#'   third in particular is needed only for backwards compatibility, and support
#'   will likely be deprecated.
#'
#'   Furthermore, this function should be exlusively used implicitly and
#'   by Zelig.
#' 
#' @param params the object to be casted
#' @param ... 
#' @return an object of type `parameters'
#' @seealso as.parameters.list as.parameters.parameters, as.parameters.default
#' @author Matt Owen \email{mowen@@ig.harvard.edu}
as.parameters <- function(params, ...)
  UseMethod("as.parameters")


#' list -> parameters
#'
#' The list may contain: 'link', 'linkinv', 'family', 'alpha', and
#' 'simulations' keys.
#'
#' @param params a list object
#' @param num an integer specifying the number of simulations
#'        to be taken
#' @param ... ignored parameters
#' @return an object of type `parameters'
#' @seealso as.parameters
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
#' This is merely an identity function when casting 'parameters' objects into
#' 'parameters'.
#'
#' @param paramss a parameters object
#' @param ... ignored parameters
#' @return the same parameter object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.parameters.parameters <- function(params, ...)
  params

#' ??? -> parameters
#' @note This, in future revisions, should throw warnings about deprecation.
#' @param params any non-supported data-type
#' @return the object passed in
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
