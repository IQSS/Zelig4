#' Param Method for the \code{tobit} Zelig Model
#' @note This method is used by the \code{tobit} Zelig model
#' @usage \method{param}{tobit}(obj, num, ...)
#' @S3method param tobit
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.tobit <- function(obj, num=1000, ...) {
  cov <- vcov(.fitted)
  mu <- c(coef(.fitted), log(.fitted$scale))

  # Return
  list(
       coef = mvrnorm(num, mu=mu, Sigma=cov),
       linkinv = NULL
       )
}
