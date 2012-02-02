#' Param Method for the \code{logit} Zelig Model
#' @note This method is used by the \code{logit} Zelig model
#' @usage \method{param}{logit}(obj, num, ...)
#' @S3method param logit
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.logit <- function(obj, num, ...) {
  list(
       simulations = mvrnorm(n=num, mu=coef(obj), Sigma=vcov(obj)),
       alpha       = NULL,
       fam = binomial(link="logit")
       )
}
