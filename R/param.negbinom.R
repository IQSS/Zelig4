#' param method for the `negbinom' Zelig model
#'
#' @S3method param negbinom
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.negbinom <- function(z, num=1000, bootstrap=NULL) {
  list(
       simulations = mvrnorm(num, mu=coef(z), Sigma=vcov(z)),
       alpha = z[["theta"]],
       link = function (e) e
       )
}
