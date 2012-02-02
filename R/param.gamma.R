#' param method for the `gamma' Zelig model
#'
#' @usage \method{param}{gamma}(obj, num, ...)
#' @S3method param gamma
#'
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a `parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.gamma <- function(obj, num, ...) {
  # shape
  shape <- gamma.shape(obj)

  alpha <- rnorm(n=num, mean=shape$alpha, sd=shape$SE)

  #
  list(
       coef  = mvrnorm(n=num, mu=coef(obj), Sigma=vcov(obj)),
       alpha = alpha,
       link  = function (x) 1/x,
       linkinv= function (x) 1/x
       )
}
