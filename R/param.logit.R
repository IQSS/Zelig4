#' param method for the `logit' Zelig model
#'
#' @S3method param logit
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.logit <- function(obj, num, ...) {
  list(
       simulations = mvrnorm(n=num, mu=coef(z), Sigma=vcov(z)),
       alpha       = NULL,
       fam = binomial(link="logit")
       )
}
