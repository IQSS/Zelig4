#' Param Method for the \code{exp} Zelig Model
#' @note This method is used by the \code{param} Zelig model
#' @usage \method{param}{exp}(obj, num, ...)
#' @S3method param exp
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.exp <- function(obj, num=1000, ...) {
  cov <- vcov(obj)
  mu <- coef(obj)

  # Return
  list(
       coef = mvrnorm(num, mu=mu, Sigma=cov),
       linkinv = survreg.distributions[["exponential"]]$itrans
       )
}
