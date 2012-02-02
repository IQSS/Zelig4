#' Param Method for the 'ls' Zelig Model
#' @note This method currently returns via a deprectated style
#' @usage \method{param}{ls}(obj, num, \dots)
#' @S3method param ls
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.ls <- function(obj, num, ...) {
  mvrnorm(n=num, mu=coef(obj), Sigma=vcov(obj))
}
