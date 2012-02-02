#' Param Method for the 'negbinom' Zelig Model
#' @note This method is used by the 'negbinom' Zelig model
#' @usage \method{param}{negbinom}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.negbinom <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, mu=coef(obj), Sigma=vcov(obj)),
       alpha = obj[["theta"]],
       link = function (e) e,
       linkinv = function (e) e
       )
}
