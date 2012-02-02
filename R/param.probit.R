#' Param Method for the 'probit' Zelig Model
#' @note This method is used by the 'probit' Zelig model
#' @usage \method{param}{probit}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.probit <- function(obj, num=1000, ...) {
  list(
       simulations = mvrnorm(n=num, mu=coef(obj), Sigma=vcov(obj)),
       alpha = NULL,
       fam = binomial(link="probit")
       )
}
