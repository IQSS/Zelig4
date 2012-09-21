#' Param Method for the \code{threesls} Zelig Model
#' @note This method is used by the \code{threesls} Zelig model
#' @usage \method{param}{threesls}(obj, num, ...)
#' @S3method param threesls
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.threesls <- function(obj, num=1000, ...) {
  list(
       coef = NULL,
       linkinv = NULL
       )
}
