#' Convert \code{call} Object to a String
#'
#' This method concerts \code{call} objects into a simple, intuitive 
#' human-readable form.
#' @usage \method{toString}{call}(x, ...)
#' @S3method toString call
#' @param x a \code{call} object
#' @param ... ignored parameters
#' @return a character-string representing the \code{call} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
toString.call <- function (x, ...)
  as.character(as.expression(x))
