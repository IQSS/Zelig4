#' Generic method for casting various object as a
#' `summarized' object
#'
#' This function is particularly for use by the
#' `summarize' method, which summarizes the simulations
#' taken from the `qi' method.
#' 
#' @param x an object
#' @param ... unspecified parameters
#' @return a `summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized <- function(x, ...) {
  UseMethod("as.summarized")
}


#' summarized.qi -> summarized.qi
#'
#' @param x an object of type `summarized.qi'
#' @param ... ignored parameters
#' @return the same `summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized.summarized.qi <- function(x, ...) {
  x
}
