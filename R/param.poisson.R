#' Param Method for the 'poisson' Zelig Model
#' @note This method is used by the 'poisson' Zelig model
#' @usage \method{param}{poisson}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.poisson <- function (obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, mu=coef(obj), Sigma=vcov(obj)),
       fam = poisson()
       )
}
