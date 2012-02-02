#' Generic Method for Casting Objectst as 'summarized' Objects
#' 
#' This function is particularly for use by the 'summarize' method, which
#' summarizes the simulations taken from the 'qi' method. The generic function
#' 'summary' when applied to a Zelig Simulation implicitly uses this function.
#' 
#' @note This is made available on the Global namespace as a matter of potential
#' future compliancy.
#' @param x an object
#' @param ... unspecified parameters
#' @return a 'summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized <- function(x, ...) {
  UseMethod("as.summarized")
}

#' summarized.qi -> summarized.qi
#' 
#' Identity operation on ``summarized.qi'' objects
#' @usage \method{as.summarized}{summarized.qi}(x, ...)
#' @param x an object of type 'summarized.qi'
#' @param ... ignored parameters
#' @return the same 'summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized.summarized.qi <- function(x, ...) {
  x
}
