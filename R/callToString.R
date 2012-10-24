#' Convert \code{call} Object to a String
#'
#' This method concerts \code{call} objects into a simple, intuitive 
#' human-readable form.
#' @param x a \code{call} object
#' @param ... ignored parameters
#' @return a character-string representing the \code{call} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
callToString <- function (x, ...)
  as.character(as.expression(x))
