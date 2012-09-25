#' param method for the `gamma' Zelig model
#'
#' Return parameter estimates for the ``gamma'' GLM in Zelig.
#' @usage \method{param}{gamma}(obj, num, ...)
#' @S3method param gamma
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a `parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.gamma <- function(obj, num = 1000, ...) {
  # Extract shape parameters, which will be used to simulate the ancillary
  # parameters
  shape <- gamma.shape(.object)

  # Simulate ancillary parameters
  alpha <- rnorm(n=num, mean=shape$alpha, sd=shape$SE)

  #
  list(
       simulations  = mvrnorm(n=num, mu=coef(.object), Sigma=vcov(.object)),
       alpha = alpha,
       family = Gamma()
       )
}
